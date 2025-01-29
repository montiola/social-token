;; Social Token Contract
;; Implements a social token with advanced features including:
;; - Minting with caps
;; - Transfer functionality
;; - Burning mechanism
;; - Token metadata
;; - Owner controls
;; - Staking capabilities

;; Constants
(define-constant contract-owner tx-sender)
(define-constant token-name "SocialToken")
(define-constant token-symbol "SOCL")
(define-constant token-decimals u6)
(define-constant max-supply u1000000000000) ;; 1 billion tokens with 6 decimals

;; Error codes
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-max-supply-reached (err u104))
(define-constant err-transfer-failed (err u105))
(define-constant err-already-staked (err u106))
(define-constant err-not-staked (err u107))
(define-constant err-invalid-recipient (err u108))

;; Data vars
(define-data-var total-supply uint u0)
(define-data-var token-uri (string-ascii 256) "https://example.com/metadata.json")

;; Data maps
(define-map balances principal uint)
(define-map allowances { owner: principal, spender: principal } uint)
(define-map staking-info { staker: principal } { amount: uint, timestamp: uint })

;; Private functions
(define-private (is-owner)
    (is-eq tx-sender contract-owner))

(define-private (transfer-internal (amount uint) (sender principal) (recipient principal))
    (let (
        (sender-balance (default-to u0 (map-get? balances sender)))
        (recipient-balance (default-to u0 (map-get? balances recipient)))
    )
    (asserts! (not (is-eq sender recipient)) err-invalid-recipient)
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (asserts! (<= (+ recipient-balance amount) max-supply) err-max-supply-reached)
    (map-set balances sender (- sender-balance amount))
    (map-set balances recipient (+ recipient-balance amount))
    (ok true)))

;; Public functions
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-owner) err-owner-only)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (not (is-eq recipient contract-owner)) err-invalid-recipient)
        (let (
            (current-supply (var-get total-supply))
            (new-supply (+ current-supply amount))
            (recipient-balance (default-to u0 (map-get? balances recipient)))
        )
        (asserts! (<= new-supply max-supply) err-max-supply-reached)
        (asserts! (<= (+ recipient-balance amount) max-supply) err-max-supply-reached)
        (var-set total-supply new-supply)
        (map-set balances recipient (+ recipient-balance amount))
        (ok true))))

(define-public (transfer (amount uint) (recipient principal))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (not (is-eq recipient tx-sender)) err-invalid-recipient)
        (transfer-internal amount tx-sender recipient)))

(define-public (approve (amount uint) (spender principal))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (not (is-eq spender tx-sender)) err-invalid-recipient)
        (map-set allowances { owner: tx-sender, spender: spender } amount)
        (ok true)))

(define-public (transfer-from (amount uint) (sender principal) (recipient principal))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (not (is-eq sender recipient)) err-invalid-recipient)
        (asserts! (not (is-eq sender tx-sender)) err-invalid-recipient)
        (let (
            (allowance (default-to u0 (map-get? allowances { owner: sender, spender: tx-sender })))
        )
        (asserts! (>= allowance amount) err-insufficient-balance)
        (map-set allowances { owner: sender, spender: tx-sender } (- allowance amount))
        (transfer-internal amount sender recipient))))

(define-public (burn (amount uint))
    (let (
        (sender-balance (default-to u0 (map-get? balances tx-sender)))
    )
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (map-set balances tx-sender (- sender-balance amount))
    (var-set total-supply (- (var-get total-supply) amount))
    (ok true)))

(define-public (stake (amount uint))
    (let (
        (sender-balance (default-to u0 (map-get? balances tx-sender)))
        (existing-stake (map-get? staking-info { staker: tx-sender }))
    )
    (asserts! (>= sender-balance amount) err-insufficient-balance)
    (asserts! (is-none existing-stake) err-already-staked)
    (map-set balances tx-sender (- sender-balance amount))
    (map-set staking-info { staker: tx-sender } { amount: amount, timestamp: block-height })
    (ok true)))

(define-public (unstake)
    (let (
        (stake-data (map-get? staking-info { staker: tx-sender }))
    )
    (asserts! (is-some stake-data) err-not-staked)
    (let (
        (stake-amount (get amount (unwrap! stake-data err-not-staked)))
        (current-balance (default-to u0 (map-get? balances tx-sender)))
    )
    (map-delete staking-info { staker: tx-sender })
    (map-set balances tx-sender (+ current-balance stake-amount))
    (ok true))))

;; Read-only functions
(define-read-only (get-name)
    token-name)

(define-read-only (get-symbol)
    token-symbol)

(define-read-only (get-decimals)
    token-decimals)

(define-read-only (get-total-supply)
    (var-get total-supply))

(define-read-only (get-balance (account principal))
    (default-to u0 (map-get? balances account)))

(define-read-only (get-allowance (owner principal) (spender principal))
    (default-to u0 (map-get? allowances { owner: owner, spender: spender })))

(define-read-only (get-token-uri)
    (var-get token-uri))

(define-read-only (get-staking-info (staker principal))
    (map-get? staking-info { staker: staker }))
