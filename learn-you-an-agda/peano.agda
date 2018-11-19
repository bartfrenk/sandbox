module peano where

data ℕ : Set where
  zero : ℕ
  succ : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + zero = zero
zero + n = n
(succ n) + n′ = succ (n + n′)

data _even : ℕ → Set where
  ZERO : zero even
  STEP : (x : ℕ) → x even → (succ (succ x)) even

proof : succ (succ (succ (succ zero))) even
proof = STEP (succ (succ zero)) (STEP zero ZERO)
