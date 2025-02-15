;; Token Voting Smart Contract
;; A simple DAO-like contract for creating and voting on proposals

;; Define token for governance
(define-fungible-token governance-token)

;; Data structures
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    proposer: principal,
    yes-votes: uint,
    no-votes: uint,
    status: (string-ascii 10),
    end-block: uint
  }
)

;; Keep track of votes
(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: (string-ascii 3) }
)

;; Keep track of next proposal ID
(define-data-var next-proposal-id uint u1)

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u403))
(define-constant ERR-PROPOSAL-EXISTS (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-PROPOSAL-CLOSED (err u103))
(define-constant ERR-INSUFFICIENT-TOKENS (err u104))

;; Read-only functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get user vote for a proposal
(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

;; Check if a proposal is still active
(define-read-only (is-proposal-active (proposal-id uint))
  (let ((proposal (unwrap! (get-proposal proposal-id) false)))
    (< block-height (get end-block proposal))
  )
)

;; Public functions

;; Initialize tokens - mint some tokens to contract deployer
(define-public (initialize-tokens)
  (begin
    (ft-mint? governance-token u1000 tx-sender)
  )
)

;; Create a new proposal
(define-public (create-proposal (title (string-utf8 100)) (description (string-utf8 500)) (duration uint))
  (let ((proposal-id (var-get next-proposal-id))
        (token-balance (ft-get-balance governance-token tx-sender)))
    
    ;; Must have at least 100 tokens to create a proposal
    (asserts! (>= token-balance u100) ERR-INSUFFICIENT-TOKENS)
    
    ;; Take 100 tokens as deposit
    (try! (ft-transfer? governance-token u100 tx-sender (as-contract tx-sender)))
    
    ;; Create the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        yes-votes: u0,
        no-votes: u0,
        status: "active",
        end-block: (+ block-height duration)
      }
    )
    
    ;; Increment the proposal ID
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote (proposal-id uint) (vote-type (string-ascii 3)))
  (let ((proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (token-balance (ft-get-balance governance-token tx-sender)))
    
    ;; Check if proposal is still active
    (asserts! (< block-height (get end-block proposal)) ERR-PROPOSAL-CLOSED)
    
    ;; Check if user has already voted
    (asserts! (is-none (get-vote proposal-id tx-sender)) ERR-ALREADY-VOTED)
    
    ;; Must have at least 1 token to vote
    (asserts! (> token-balance u0) ERR-INSUFFICIENT-TOKENS)
    
    ;; Record the vote weight (1 token = 1 vote)
    (map-set votes 
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-type }
    )
    
    ;; Update the vote tally
    (if (is-eq vote-type "yes")
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { yes-votes: (+ (get yes-votes proposal) token-balance) })
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { no-votes: (+ (get no-votes proposal) token-balance) }) 
      )
    )
    
    (ok true)
  )
)

;; Execute a proposal once voting ends
(define-public (finalize-proposal (proposal-id uint))
  (let ((proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    
    ;; Check if proposal has ended
    (asserts! (>= block-height (get end-block proposal)) ERR-PROPOSAL-CLOSED)
    
    ;; Check if already finalized
    (asserts! (is-eq (get status proposal) "active") (err u105))
    
    ;; Update status based on votes
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal 
        { status: (if (> (get yes-votes proposal) (get no-votes proposal))
                     "passed"
                     "failed") }
      )
    )
    
    ;; Return deposit to proposer
    (as-contract (ft-transfer? governance-token u100 (as-contract tx-sender) (get proposer proposal)))
    
    (ok true)
  )
)

;; Function to transfer tokens to another user
(define-public (transfer-tokens (recipient principal) (amount uint))
  (ft-transfer? governance-token amount tx-sender recipient)
)
//