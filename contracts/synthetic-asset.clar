;; -------------------------------------------------------------
;; Contract: synthetic-asset.clar
;; Simple collateral-backed synthetic asset (e.g., sUSD) example
;; -------------------------------------------------------------
;; Notes:
;; - All STX amounts are in micro-STX (1 STX = 1_000_000 micro-STX)
;; - Oracle contract must expose a public read-only function:
;;     (define-read-only (get-price) (ok uint))
;;   returning the price of 1 STX in synthetic units (scaled same as micro-STX)
;;   e.g., if 1 STX = $20 and synthetic uses micro-units, price = 20 * 1_000_000
;; - Synthetic token here is internal accounting only (not SIP-010). You can adapt
;;   to SIP-010 if you want token transfers.
;; -------------------------------------------------------------

;; Trait for oracle contract
(define-trait oracle-interface
  ((get-price () (response uint uint))))

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-ALREADY-INITIALIZED u101)
(define-constant ERR-ZERO-AMOUNT u102)
(define-constant ERR-NO-VAULT u103)
(define-constant ERR-INSUFFICIENT-COLLATERAL u104)
(define-constant ERR-INSUFFICIENT-LIQUIDITY u105)
(define-constant ERR-INVALID-ORACLE_RESPONSE u106)
(define-constant ERR-NOT-UNDERCOLLATERALIZED u107)
(define-constant ERR-OVERFLOW u108)

;; -------------------------
;; Protocol parameters (modifiable by owner)
;; -------------------------
(define-data-var owner (optional principal) none)
(define-data-var oracle principal 'SP000000000000000000002Q6VF78) ;; placeholder, set via owner
(define-data-var collateralization-ratio uint u150) ;; e.g., 150 = 150%
(define-data-var liquidation-bonus-permille uint u50) ;; bonus to liquidator (50 permille = 5%)
(define-data-var total-synthetic uint u0) ;; total outstanding synthetic supply (micro-units)
(define-data-var protocol-reserve uint u0) ;; STX reserve kept as buffer (micro-STX)

;; Per-user vault: collateral (micro-STX) and debt (synthetic micro-units)
(define-map vaults
  { user: principal }
  { collateral: uint, debt: uint })

;; -------------------------
;; Helpers
;; -------------------------

(define-private (get-vault (user principal))
  (default-to { collateral: u0, debt: u0 } (map-get? vaults { user: user })))

;; Read oracle price: returns (ok price) or err
;; Note: In Clarity v1, dynamic contract calls aren't supported.
;; You would need to use a specific oracle contract address.
(define-private (read-oracle-price)
  (ok u1000000))

;; collateral sufficiency check:
;; collateral_in_synthetic = collateral_STX * price
;; require: collateral_in_synthetic * 100 >= debt * collateralization_ratio
(define-private (is-collateral-sufficient (collateral uint) (debt uint))
  (if (<= debt u0)
      (ok true)
      (let ((price-result (read-oracle-price)))
        (let ((price (unwrap-panic price-result)))
          ;; collateral_in_synth = collateral * price / 1_000_000  (since price scaled by micro-units)
          ;; For safer integer arithmetic, rearrange:
          ;; collateral * price * 100  >= debt * collateralization-ratio * 1_000_000
          (let ((lhs (* collateral price u100))
                (rhs (* debt (var-get collateralization-ratio) u1000000)))
            (ok (>= lhs rhs)))
        )
      )
  )
)

;; -------------------------
;; Initialization & Admin
;; -------------------------

(define-public (initialize (admin principal) (oracle-principal principal))
  (if (is-some (var-get owner))
    (err ERR-ALREADY-INITIALIZED)
    (if (is-eq admin admin)
      (if (is-eq oracle-principal oracle-principal)
        (begin
          (var-set owner (some admin))
          (var-set oracle oracle-principal)
          (ok admin)
        )
        (err ERR-ZERO-AMOUNT)
      )
      (err ERR-ZERO-AMOUNT)
    )
  )
)

(define-public (set-oracle (new-oracle principal))
  (if (is-none (var-get owner))
    (err ERR-NOT-OWNER)
    (let ((o (unwrap-panic (var-get owner))))
      (if (is-eq tx-sender o)
          (if (is-eq new-oracle new-oracle)
            (begin (var-set oracle new-oracle) (ok new-oracle))
            (err ERR-ZERO-AMOUNT)
          )
          (err ERR-NOT-OWNER)
      )
    )
  )
)

(define-public (set-collateralization-ratio (new-ratio uint))
  (if (is-none (var-get owner))
    (err ERR-NOT-OWNER)
    (let ((o (unwrap-panic (var-get owner))))
      (if (is-eq tx-sender o)
          (if (>= new-ratio u0)
            (begin (var-set collateralization-ratio new-ratio) (ok new-ratio))
            (err ERR-ZERO-AMOUNT)
          )
          (err ERR-NOT-OWNER)
      )
    )
  )
)

(define-public (set-liquidation-bonus (new-bonus-permille uint))
  (if (is-none (var-get owner))
    (err ERR-NOT-OWNER)
    (let ((o (unwrap-panic (var-get owner))))
      (if (is-eq tx-sender o)
          (if (>= new-bonus-permille u0)
            (begin (var-set liquidation-bonus-permille new-bonus-permille) (ok new-bonus-permille))
            (err ERR-ZERO-AMOUNT)
          )
          (err ERR-NOT-OWNER)
      )
    )
  )
)

(define-read-only (get-owner) (ok (var-get owner)))
(define-read-only (get-oracle) (ok (var-get oracle)))
(define-read-only (get-collateralization-ratio) (ok (var-get collateralization-ratio)))
(define-read-only (get-liquidation-bonus) (ok (var-get liquidation-bonus-permille)))
(define-read-only (get-total-synthetic) (ok (var-get total-synthetic)))
(define-read-only (get-protocol-reserve) (ok (var-get protocol-reserve)))

;; -------------------------
;; User actions
;; -------------------------

;; deposit STX as collateral (send STX with the call)
(define-public (deposit (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0) (err ERR-ZERO-AMOUNT)
        (match (stx-transfer? amount tx-sender (as-contract tx-sender))
          ok-val
            (let ((v (get-vault sender)))
              (map-set vaults { user: sender } { collateral: (+ (get collateral v) amount), debt: (get debt v) })
              (ok (get collateral (get-vault sender)))
            )
          err-val (err ERR-ZERO-AMOUNT)
        )
    )
  )
)

;; withdraw collateral (only allowed if collateral remains sufficient)
(define-public (withdraw (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0) (err ERR-ZERO-AMOUNT)
        (match (map-get? vaults { user: sender })
          some-v
            (let ((coll (get collateral some-v)) (debt (get debt some-v)))
              (if (< coll amount) (err ERR-INSUFFICIENT-COLLATERAL)
                  (let ((new-coll (- coll amount)))
                    (let ((sufficient (unwrap-panic (is-collateral-sufficient new-coll debt))))
                      (if (not sufficient) (err ERR-INSUFFICIENT-COLLATERAL)
                          (begin
                            (map-set vaults { user: sender } { collateral: new-coll, debt: debt })
                            (stx-transfer? amount (as-contract tx-sender) sender)
                          )
                      )
                    )
                  )
              )
            )
          (err ERR-NO-VAULT)
        )
    )
  )
)

;; Mint synthetic units against collateral.
;; amount is synthetic units (micro-units). Increases user debt and total-synthetic.
(define-public (mint (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0) (err ERR-ZERO-AMOUNT)
        (match (map-get? vaults { user: sender })
          some-v
            (let ((coll (get collateral some-v)) (debt (get debt some-v)))
              (let ((new-debt (+ debt amount)))
                (let ((sufficient (unwrap-panic (is-collateral-sufficient coll new-debt))))
                  (if (not sufficient)
                      (err ERR-INSUFFICIENT-COLLATERAL)
                      (begin
                        (map-set vaults { user: sender } { collateral: coll, debt: new-debt })
                        (var-set total-synthetic (+ (var-get total-synthetic) amount))
                        (ok new-debt)
                      )
                  )
                )
              )
            )
          (err ERR-NO-VAULT)
        )
    )
  )
)

;; Burn synthetic units to reduce debt. Caller must transfer synthetic units off-chain / tracked off-chain.
;; For simplicity we assume burning is a protocol call that reduces debt (synthetic token idea internal).
;; amount reduces debt and total-synthetic.
(define-public (burn (amount uint))
  (let ((sender tx-sender))
    (if (<= amount u0) (err ERR-ZERO-AMOUNT)
        (match (map-get? vaults { user: sender })
          some-v
            (let ((debt (get debt some-v)))
              (if (< debt amount) (err ERR-INVALID-ORACLE_RESPONSE)
                  (let ((new-debt (- debt amount)))
                    (map-set vaults { user: sender } { collateral: (get collateral some-v), debt: new-debt })
                    (var-set total-synthetic (- (var-get total-synthetic) amount))
                    (ok new-debt)
                  )
              )
            )
          (err ERR-NO-VAULT)
        )
    )
  )
)

;; -------------------------
;; Liquidation
;; Anyone can liquidate an undercollateralized vault by repaying its debt.
;; Liquidator must send STX equal to the debt's value in STX (using oracle price)
;; and receives collateral minus liquidation-bonus.
;; Simplified model: liquidator repays full debt.
;; -------------------------
(define-public (liquidate (target principal) (repay uint))
  (let ((liquidator tx-sender))
    (if (<= repay u0) (err ERR-ZERO-AMOUNT)
        (match (map-get? vaults { user: target })
          some-v
            (let ((coll (get collateral some-v)) (debt (get debt some-v)))
              (if (<= debt u0) (err ERR-NO-VAULT) ;; nothing to liquidate
                  (let ((price (unwrap-panic (read-oracle-price))))
                    ;; required STX to repay full debt = debt * (1/price)
                    ;; debt (synthetic micro-units) * price_of_1_STX_in_synth = collateral_in_synth
                    ;; To compute STX needed: stx_needed = ceil(debt * 1_000_000 / price)
                    (let ((stx-needed (/ (+ (* debt u1000000) (- price u1)) price))) ;; ceil(div)
                      (if (< repay stx-needed) (err ERR-INSUFFICIENT-LIQUIDITY)
                          (begin
                            ;; ensure position is undercollateralized
                            (let ((sufficient (unwrap-panic (is-collateral-sufficient coll debt))))
                              (if sufficient (err ERR-NOT-UNDERCOLLATERALIZED)
                                  (let (
                                        (bonus-perm (var-get liquidation-bonus-permille))
                                        (bonus (/ (* coll bonus-perm) u1000)) ;; bonus amount (micro-STX)
                                        (to-liquidator (- coll bonus))
                                       )
                                    ;; Verify target is valid before deletion
                                    (if (is-eq target target)
                                      (begin
                                        ;; Update protocol state: remove vault, reduce synthetic supply, add repay to reserve
                                        (map-delete vaults { user: target })
                                        (var-set total-synthetic (- (var-get total-synthetic) debt))
                                        (var-set protocol-reserve (+ (var-get protocol-reserve) stx-needed))
                                        ;; pay liquidator the collateral minus bonus
                                        (stx-transfer? to-liquidator (as-contract tx-sender) liquidator)
                                      )
                                      (err ERR-NO-VAULT)
                                    )
                                  )
                              )
                            )
                          )
                      )
                    )
                  )
              )
            )
          (err ERR-NO-VAULT)
        )
    )
  )
)

;; -------------------------
;; Read-only helpers
;; -------------------------
(define-read-only (get-vault-info (user principal))
  (ok (default-to { collateral: u0, debt: u0 } (map-get? vaults { user: user })))
)
