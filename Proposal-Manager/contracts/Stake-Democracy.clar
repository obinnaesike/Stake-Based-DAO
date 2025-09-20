;; DECENTRALIZED COMMUNITY GOVERNANCE SMART CONTRACT
;; A comprehensive voting platform for decentralized community decision-making
;; Features weighted voting, stake-based proposal creation, and transparent governance

;; CONTRACT ADMINISTRATION
(define-constant governance-contract-administrator-address tx-sender)

;; ERROR CONSTANTS AND DEFINITIONS

;; Authentication and Authorization Errors
(define-constant ERR-UNAUTHORIZED-ACCESS-DENIED (err u100))
(define-constant ERR-FORBIDDEN-ACTION-ATTEMPTED (err u102))

;; Proposal Management Errors
(define-constant ERR-PROPOSAL-NOT-FOUND-IN-SYSTEM (err u101))
(define-constant ERR-PROPOSAL-ALREADY-EXISTS-ERROR (err u106))
(define-constant ERR-PROPOSAL-CREATION-FAILED-ERROR (err u110))

;; Voting Process Errors
(define-constant ERR-VOTING-PERIOD-HAS-EXPIRED (err u103))
(define-constant ERR-DUPLICATE-VOTE-SUBMISSION-ATTEMPT (err u104))
(define-constant ERR-INVALID-VOTE-CHOICE-PROVIDED (err u105))
(define-constant ERR-ACTIVE-VOTING-STILL-IN-PROGRESS (err u107))

;; Stake and Balance Errors
(define-constant ERR-INSUFFICIENT-STAKE-BALANCE-ERROR (err u108))

;; Parameter Validation Errors
(define-constant ERR-INVALID-PARAMETER-VALUE-PROVIDED (err u109))

;; VALIDATION CONSTANTS AND SYSTEM PARAMETERS
(define-constant maximum-proposal-title-character-length u100)
(define-constant maximum-proposal-description-character-length u500)
(define-constant minimum-voting-period-duration-in-blocks u144) ;; ~1 day minimum
(define-constant maximum-voting-period-duration-in-blocks u4032) ;; ~4 weeks maximum
(define-constant default-community-member-voting-power u1)
(define-constant administrator-voting-power-multiplier u10)
(define-constant maximum-individual-member-voting-power u1000)

;; SYSTEM STATE VARIABLES
(define-data-var next-proposal-unique-identifier-counter uint u1)
(define-data-var minimum-proposal-submission-stake-requirement uint u1000000) ;; 1 STX
(define-data-var standard-voting-period-duration-in-blocks uint u1008) ;; ~1 week in blocks
(define-data-var governance-system-currently-active-status bool true)

;; CORE DATA STRUCTURES AND MAPPINGS
;; Primary proposal storage with comprehensive metadata
(define-map community-governance-proposal-registry
  { proposal-unique-identifier: uint }
  {
    proposal-display-title: (string-ascii 100),
    proposal-detailed-description: (string-ascii 500),
    proposal-creator-wallet-address: principal,
    voting-period-start-block-height: uint,
    voting-period-end-block-height: uint,
    total-affirmative-votes-accumulated-count: uint,
    total-negative-votes-accumulated-count: uint,
    unique-voting-participants-count: uint,
    proposal-currently-active-status: bool,
    creator-submitted-stake-amount: uint,
    proposal-creation-block-height: uint
  }
)

;; Individual voting records with detailed metadata
(define-map community-member-voting-record-registry
  { proposal-unique-identifier: uint, voting-participant-wallet-address: principal }
  {
    submitted-vote-choice-boolean: bool, ;; true = affirmative, false = negative
    participant-calculated-voting-weight: uint,
    vote-submission-block-height: uint,
    vote-submission-timestamp-block: uint
  }
)

;; Participant stake tracking for each proposal
(define-map proposal-participant-stake-tracking-registry
  { proposal-unique-identifier: uint, participant-wallet-address: principal }
  uint
)

;; Community member voting power assignments
(define-map community-member-voting-power-assignment-registry
  { member-wallet-address: principal }
  {
    assigned-individual-voting-weight: uint,
    voting-power-assignment-block-height: uint,
    member-verification-status-boolean: bool
  }
)

;; Final proposal resolution and outcome tracking
(define-map proposal-final-resolution-outcome-registry
  { proposal-unique-identifier: uint }
  {
    proposal-fully-resolved-status-boolean: bool,
    final-resolution-block-height: uint,
    determined-winning-outcome-string: (string-ascii 10),
    total-community-participation-count: uint
  }
)

;; READ-ONLY QUERY FUNCTIONS
(define-read-only (get-proposal-comprehensive-details-by-identifier (proposal-identifier uint))
  (map-get? community-governance-proposal-registry { proposal-unique-identifier: proposal-identifier })
)

(define-read-only (get-participant-voting-record-by-identifier (proposal-identifier uint) (participant-wallet-address principal))
  (map-get? community-member-voting-record-registry { proposal-unique-identifier: proposal-identifier, voting-participant-wallet-address: participant-wallet-address })
)

(define-read-only (get-participant-staked-amount-by-identifier (proposal-identifier uint) (participant-wallet-address principal))
  (default-to u0 (map-get? proposal-participant-stake-tracking-registry { proposal-unique-identifier: proposal-identifier, participant-wallet-address: participant-wallet-address }))
)

(define-read-only (get-member-current-voting-power-by-address (member-wallet-address principal))
  (match (map-get? community-member-voting-power-assignment-registry { member-wallet-address: member-wallet-address })
    member-voting-power-data (get assigned-individual-voting-weight member-voting-power-data)
    default-community-member-voting-power
  )
)

(define-read-only (get-member-verification-status-by-address (member-wallet-address principal))
  (match (map-get? community-member-voting-power-assignment-registry { member-wallet-address: member-wallet-address })
    member-voting-power-data (get member-verification-status-boolean member-voting-power-data)
    false
  )
)

(define-read-only (get-current-proposal-counter-value)
  (var-get next-proposal-unique-identifier-counter)
)

(define-read-only (get-minimum-stake-requirement-amount-in-ustx)
  (var-get minimum-proposal-submission-stake-requirement)
)

(define-read-only (get-standard-voting-period-duration-in-blocks)
  (var-get standard-voting-period-duration-in-blocks)
)

(define-read-only (get-governance-system-operational-status)
  (var-get governance-system-currently-active-status)
)

(define-read-only (check-if-voting-period-currently-active-by-identifier (proposal-identifier uint))
  (match (get-proposal-comprehensive-details-by-identifier proposal-identifier)
    proposal-detailed-information 
      (and 
        (get proposal-currently-active-status proposal-detailed-information)
        (<= stacks-block-height (get voting-period-end-block-height proposal-detailed-information))
        (>= stacks-block-height (get voting-period-start-block-height proposal-detailed-information))
        (var-get governance-system-currently-active-status)
      )
    false
  )
)

(define-read-only (calculate-comprehensive-proposal-outcome-by-identifier (proposal-identifier uint))
  (match (get-proposal-comprehensive-details-by-identifier proposal-identifier)
    proposal-detailed-information
      (let 
        (
          (total-affirmative-votes-count (get total-affirmative-votes-accumulated-count proposal-detailed-information))
          (total-negative-votes-count (get total-negative-votes-accumulated-count proposal-detailed-information))
          (combined-total-votes-count (+ total-affirmative-votes-count total-negative-votes-count))
        )
        (ok {
          proposal-unique-identifier: proposal-identifier,
          proposal-display-title: (get proposal-display-title proposal-detailed-information),
          counted-affirmative-votes: total-affirmative-votes-count,
          counted-negative-votes: total-negative-votes-count,
          unique-participants-total-count: (get unique-voting-participants-count proposal-detailed-information),
          voting-period-currently-active-status: (check-if-voting-period-currently-active-by-identifier proposal-identifier),
          determined-outcome-result-string: (if (> total-affirmative-votes-count total-negative-votes-count) "approved" "rejected"),
          calculated-participation-percentage-rate: (if (> combined-total-votes-count u0) (/ (* total-affirmative-votes-count u100) combined-total-votes-count) u0)
        })
      )
    ERR-PROPOSAL-NOT-FOUND-IN-SYSTEM
  )
)

;; UTILITY AND HELPER FUNCTIONS
(define-private (increment-proposal-counter-and-get-next-identifier)
  (let ((current-proposal-counter-value (var-get next-proposal-unique-identifier-counter)))
    (var-set next-proposal-unique-identifier-counter (+ current-proposal-counter-value u1))
    current-proposal-counter-value
  )
)

(define-private (validate-proposal-submission-parameters (proposal-title-string (string-ascii 100)) (proposal-description-string (string-ascii 500)))
  (and
    (> (len proposal-title-string) u0)
    (> (len proposal-description-string) u0)
    (<= (len proposal-title-string) maximum-proposal-title-character-length)
    (<= (len proposal-description-string) maximum-proposal-description-character-length)
  )
)

(define-private (calculate-voting-period-end-block-height (voting-start-block-height uint))
  (+ voting-start-block-height (var-get standard-voting-period-duration-in-blocks))
)

;; ADMINISTRATIVE MANAGEMENT FUNCTIONS
(define-public (update-community-member-voting-power-assignment 
  (target-member-wallet-address principal) 
  (new-assigned-voting-weight uint) 
  (member-verification-status-boolean bool))
  (begin
    (asserts! (is-eq tx-sender governance-contract-administrator-address) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (> new-assigned-voting-weight u0) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    (asserts! (<= new-assigned-voting-weight maximum-individual-member-voting-power) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    ;; Validate principal is not the zero principal and is a valid address
    (asserts! (not (is-eq target-member-wallet-address 'SP000000000000000000002Q6VF78)) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    (ok (map-set community-member-voting-power-assignment-registry 
      { member-wallet-address: target-member-wallet-address }
      {
        assigned-individual-voting-weight: new-assigned-voting-weight,
        voting-power-assignment-block-height: stacks-block-height,
        member-verification-status-boolean: member-verification-status-boolean
      }
    ))
  )
)

(define-public (adjust-minimum-proposal-stake-requirement-amount (new-minimum-stake-amount-ustx uint))
  (begin
    (asserts! (is-eq tx-sender governance-contract-administrator-address) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (> new-minimum-stake-amount-ustx u0) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    (ok (var-set minimum-proposal-submission-stake-requirement new-minimum-stake-amount-ustx))
  )
)

(define-public (modify-standard-voting-period-duration-blocks (new-voting-period-duration-blocks uint))
  (begin
    (asserts! (is-eq tx-sender governance-contract-administrator-address) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (asserts! (and 
      (>= new-voting-period-duration-blocks minimum-voting-period-duration-in-blocks) 
      (<= new-voting-period-duration-blocks maximum-voting-period-duration-in-blocks)
    ) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    (ok (var-set standard-voting-period-duration-in-blocks new-voting-period-duration-blocks))
  )
)

(define-public (toggle-governance-system-operational-status (new-system-active-status bool))
  (begin
    (asserts! (is-eq tx-sender governance-contract-administrator-address) ERR-UNAUTHORIZED-ACCESS-DENIED)
    (ok (var-set governance-system-currently-active-status new-system-active-status))
  )
)

;; CORE GOVERNANCE FUNCTIONALITY
(define-public (submit-new-community-governance-proposal 
  (proposal-title-string (string-ascii 100)) 
  (proposal-description-string (string-ascii 500)))
  (let 
    (
      (new-proposal-unique-identifier (increment-proposal-counter-and-get-next-identifier))
      (voting-start-block-height (+ stacks-block-height u1))
      (voting-end-block-height (calculate-voting-period-end-block-height voting-start-block-height))
      (required-stake-amount-ustx (var-get minimum-proposal-submission-stake-requirement))
      (proposal-creator-wallet-address tx-sender)
    )
    ;; System status validation
    (asserts! (var-get governance-system-currently-active-status) ERR-FORBIDDEN-ACTION-ATTEMPTED)
    
    ;; Proposal parameter validation
    (asserts! (validate-proposal-submission-parameters proposal-title-string proposal-description-string) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    
    ;; Process required stake transfer
    (try! (stx-transfer? required-stake-amount-ustx proposal-creator-wallet-address (as-contract tx-sender)))
    
    ;; Create comprehensive proposal record
    (map-set community-governance-proposal-registry
      { proposal-unique-identifier: new-proposal-unique-identifier }
      {
        proposal-display-title: proposal-title-string,
        proposal-detailed-description: proposal-description-string,
        proposal-creator-wallet-address: proposal-creator-wallet-address,
        voting-period-start-block-height: voting-start-block-height,
        voting-period-end-block-height: voting-end-block-height,
        total-affirmative-votes-accumulated-count: u0,
        total-negative-votes-accumulated-count: u0,
        unique-voting-participants-count: u0,
        proposal-currently-active-status: true,
        creator-submitted-stake-amount: required-stake-amount-ustx,
        proposal-creation-block-height: stacks-block-height
      }
    )
    
    ;; Record creator stake information
    (map-set proposal-participant-stake-tracking-registry
      { proposal-unique-identifier: new-proposal-unique-identifier, participant-wallet-address: proposal-creator-wallet-address }
      required-stake-amount-ustx
    )
    
    (ok new-proposal-unique-identifier)
  )
)

(define-public (cast-community-governance-vote-by-identifier (proposal-identifier uint) (vote-choice-boolean bool))
  (begin
    ;; Validate proposal identifier range
    (asserts! (and 
      (> proposal-identifier u0) 
      (< proposal-identifier (var-get next-proposal-unique-identifier-counter))
    ) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    
    (let 
      (
        (validated-proposal-identifier proposal-identifier)
        (voting-participant-wallet-address tx-sender)
        (participant-calculated-voting-weight (get-member-current-voting-power-by-address voting-participant-wallet-address))
        (current-proposal-detailed-data (unwrap! (get-proposal-comprehensive-details-by-identifier validated-proposal-identifier) ERR-PROPOSAL-NOT-FOUND-IN-SYSTEM))
      )
      ;; Validate voting eligibility and conditions
      (asserts! (check-if-voting-period-currently-active-by-identifier validated-proposal-identifier) ERR-VOTING-PERIOD-HAS-EXPIRED)
      (asserts! (is-none (get-participant-voting-record-by-identifier validated-proposal-identifier voting-participant-wallet-address)) ERR-DUPLICATE-VOTE-SUBMISSION-ATTEMPT)
      (asserts! (> participant-calculated-voting-weight u0) ERR-INSUFFICIENT-STAKE-BALANCE-ERROR)
      
      ;; Record participant vote with metadata
      (map-set community-member-voting-record-registry
        { proposal-unique-identifier: validated-proposal-identifier, voting-participant-wallet-address: voting-participant-wallet-address }
        {
          submitted-vote-choice-boolean: vote-choice-boolean,
          participant-calculated-voting-weight: participant-calculated-voting-weight,
          vote-submission-block-height: stacks-block-height,
          vote-submission-timestamp-block: stacks-block-height
        }
      )
      
      ;; Update proposal vote tallies accordingly
      (map-set community-governance-proposal-registry
        { proposal-unique-identifier: validated-proposal-identifier }
        (merge current-proposal-detailed-data
          {
            total-affirmative-votes-accumulated-count: (if vote-choice-boolean 
              (+ (get total-affirmative-votes-accumulated-count current-proposal-detailed-data) participant-calculated-voting-weight)
              (get total-affirmative-votes-accumulated-count current-proposal-detailed-data)
            ),
            total-negative-votes-accumulated-count: (if vote-choice-boolean
              (get total-negative-votes-accumulated-count current-proposal-detailed-data)
              (+ (get total-negative-votes-accumulated-count current-proposal-detailed-data) participant-calculated-voting-weight)
            ),
            unique-voting-participants-count: (+ (get unique-voting-participants-count current-proposal-detailed-data) u1)
          }
        )
      )
      
      (ok true)
    )
  )
)

(define-public (finalize-proposal-voting-process-by-identifier (proposal-identifier uint))
  (begin
    ;; Validate proposal identifier range
    (asserts! (and 
      (> proposal-identifier u0) 
      (< proposal-identifier (var-get next-proposal-unique-identifier-counter))
    ) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    
    (let 
      (
        (validated-proposal-identifier proposal-identifier)
        (proposal-comprehensive-detailed-information (unwrap! (get-proposal-comprehensive-details-by-identifier validated-proposal-identifier) ERR-PROPOSAL-NOT-FOUND-IN-SYSTEM))
        (proposal-creator-wallet-address (get proposal-creator-wallet-address proposal-comprehensive-detailed-information))
        (creator-submitted-stake-amount-ustx (get creator-submitted-stake-amount proposal-comprehensive-detailed-information))
        (total-affirmative-votes-count (get total-affirmative-votes-accumulated-count proposal-comprehensive-detailed-information))
        (total-negative-votes-count (get total-negative-votes-accumulated-count proposal-comprehensive-detailed-information))
        (unique-voting-participant-count (get unique-voting-participants-count proposal-comprehensive-detailed-information))
      )
      ;; Authorization verification
      (asserts! 
        (or 
          (is-eq tx-sender proposal-creator-wallet-address)
          (is-eq tx-sender governance-contract-administrator-address)
        ) 
        ERR-UNAUTHORIZED-ACCESS-DENIED
      )
      
      ;; Validate voting period completion
      (asserts! (> stacks-block-height (get voting-period-end-block-height proposal-comprehensive-detailed-information)) ERR-ACTIVE-VOTING-STILL-IN-PROGRESS)
      ;; Validate proposal active status
      (asserts! (get proposal-currently-active-status proposal-comprehensive-detailed-information) ERR-FORBIDDEN-ACTION-ATTEMPTED)
      
      ;; Deactivate proposal status
      (map-set community-governance-proposal-registry
        { proposal-unique-identifier: validated-proposal-identifier }
        (merge proposal-comprehensive-detailed-information { proposal-currently-active-status: false })
      )
      
      ;; Record comprehensive final resolution
      (map-set proposal-final-resolution-outcome-registry
        { proposal-unique-identifier: validated-proposal-identifier }
        {
          proposal-fully-resolved-status-boolean: true,
          final-resolution-block-height: stacks-block-height,
          determined-winning-outcome-string: (if (> total-affirmative-votes-count total-negative-votes-count) "approved" "rejected"),
          total-community-participation-count: unique-voting-participant-count
        }
      )
      
      ;; Process stake return based on final outcome
      (if (> total-affirmative-votes-count total-negative-votes-count)
        ;; Proposal approved - return stake to creator
        (try! (as-contract (stx-transfer? creator-submitted-stake-amount-ustx tx-sender proposal-creator-wallet-address)))
        ;; Proposal rejected - stake remains in contract treasury
        true
      )
      
      (ok true)
    )
  )
)

(define-public (emergency-proposal-termination-procedure-by-identifier (proposal-identifier uint))
  (begin
    ;; Restrict to administrator access only
    (asserts! (is-eq tx-sender governance-contract-administrator-address) ERR-UNAUTHORIZED-ACCESS-DENIED)
    ;; Validate proposal identifier range
    (asserts! (and 
      (> proposal-identifier u0) 
      (< proposal-identifier (var-get next-proposal-unique-identifier-counter))
    ) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    
    (let 
      (
        (validated-proposal-identifier proposal-identifier)
        (proposal-comprehensive-detailed-information (unwrap! (get-proposal-comprehensive-details-by-identifier validated-proposal-identifier) ERR-PROPOSAL-NOT-FOUND-IN-SYSTEM))
        (proposal-creator-wallet-address (get proposal-creator-wallet-address proposal-comprehensive-detailed-information))
        (creator-submitted-stake-amount-ustx (get creator-submitted-stake-amount proposal-comprehensive-detailed-information))
        (unique-voting-participant-count (get unique-voting-participants-count proposal-comprehensive-detailed-information))
      )
      
      ;; Validate proposal data integrity
      (asserts! (get proposal-currently-active-status proposal-comprehensive-detailed-information) ERR-FORBIDDEN-ACTION-ATTEMPTED)
      (asserts! (> creator-submitted-stake-amount-ustx u0) ERR-INSUFFICIENT-STAKE-BALANCE-ERROR)
      
      ;; Immediately deactivate proposal
      (map-set community-governance-proposal-registry
        { proposal-unique-identifier: validated-proposal-identifier }
        (merge proposal-comprehensive-detailed-information { proposal-currently-active-status: false })
      )
      
      ;; Record emergency resolution status
      (map-set proposal-final-resolution-outcome-registry
        { proposal-unique-identifier: validated-proposal-identifier }
        {
          proposal-fully-resolved-status-boolean: true,
          final-resolution-block-height: stacks-block-height,
          determined-winning-outcome-string: "terminated",
          total-community-participation-count: unique-voting-participant-count
        }
      )
      
      ;; Return stake to creator in emergency situations
      (try! (as-contract (stx-transfer? creator-submitted-stake-amount-ustx tx-sender proposal-creator-wallet-address)))
      
      (ok true)
    )
  )
)

(define-public (withdraw-participant-staked-amount-by-identifier (proposal-identifier uint))
  (begin
    ;; Validate proposal identifier range
    (asserts! (and 
      (> proposal-identifier u0) 
      (< proposal-identifier (var-get next-proposal-unique-identifier-counter))
    ) ERR-INVALID-PARAMETER-VALUE-PROVIDED)
    
    (let 
      (
        (validated-proposal-identifier proposal-identifier)
        (stake-withdrawing-participant-address tx-sender)
        (proposal-comprehensive-detailed-information (unwrap! (get-proposal-comprehensive-details-by-identifier validated-proposal-identifier) ERR-PROPOSAL-NOT-FOUND-IN-SYSTEM))
        (participant-staked-amount-ustx (get-participant-staked-amount-by-identifier validated-proposal-identifier stake-withdrawing-participant-address))
      )
      ;; Validate withdrawal eligibility conditions
      (asserts! (not (check-if-voting-period-currently-active-by-identifier validated-proposal-identifier)) ERR-ACTIVE-VOTING-STILL-IN-PROGRESS)
      (asserts! (> participant-staked-amount-ustx u0) ERR-INSUFFICIENT-STAKE-BALANCE-ERROR)
      
      ;; Clear participant stake record
      (map-delete proposal-participant-stake-tracking-registry { proposal-unique-identifier: validated-proposal-identifier, participant-wallet-address: stake-withdrawing-participant-address })
      
      ;; Transfer stake amount back to participant
      (try! (as-contract (stx-transfer? participant-staked-amount-ustx tx-sender stake-withdrawing-participant-address)))
      
      (ok participant-staked-amount-ustx)
    )
  )
)

;; CONTRACT INITIALIZATION PROCEDURE

(begin
  ;; Initialize administrator with enhanced voting power and verification
  (map-set community-member-voting-power-assignment-registry 
    { member-wallet-address: governance-contract-administrator-address } 
    {
      assigned-individual-voting-weight: administrator-voting-power-multiplier,
      voting-power-assignment-block-height: stacks-block-height,
      member-verification-status-boolean: true
    }
  )
)