class HomogenousTuple α where
  type family TupleItem α
  tupleToList ∷ α → [TupleItem α]

instance HomogenousTuple (α,α) where
  type instance TupleItem (α,α) = α
  tupleToList (a0,a1) = [a0,a1]

instance HomogenousTuple (α,α,α) where
  type instance TupleItem (α,α,α) = α
  tupleToList (a0,a1,a2) = [a0,a1,a2]

instance HomogenousTuple (α,α,α,α) where
  type instance TupleItem (α,α,α,α) = α
  tupleToList (a0,a1,a2,a3) = [a0,a1,a2,a3]

instance HomogenousTuple (α,α,α,α,α) where
  type instance TupleItem (α,α,α,α,α) = α
  tupleToList (a0,a1,a2,a3,a4) = [a0,a1,a2,a3,a4]

instance HomogenousTuple (α,α,α,α,α,α) where
  type instance TupleItem (α,α,α,α,α,α) = α
  tupleToList (a0,a1,a2,a3,a4,a5) = [a0,a1,a2,a3,a4,a5]

------------------------------------------------------------

class TuplePrepend α β γ where
  type family TuplePrepended α β
  tuplePrepend ∷ α → β → γ
  (⨤) ∷ α → β → γ
  (⨤) = tuplePrepend

instance ∀ α β γ . TuplePrepend α (β,γ) (α,β,γ) where
  type instance TuplePrepended α (β,γ) = (α,β,γ)
  tuplePrepend α (β,γ) = (α,β,γ)

instance ∀ α β γ δ . TuplePrepend α (β,γ,δ) (α,β,γ,δ) where
  type instance TuplePrepended α (β,γ,δ) = (α,β,γ,δ)
  tuplePrepend α (β,γ,δ) = (α,β,γ,δ)

------------------------------------------------------------

class TupleAppend α β γ where
  type family TupleAppended α β
  tupleAppend ∷ α → β → γ
  (⨦) ∷ α → β → γ
  (⨦) = tupleAppend

instance ∀ α β γ . TupleAppend (α,β) γ (α,β,γ) where
  type instance TupleAppended (α,β) γ = (α,β,γ)
  tupleAppend (α,β) γ = (α,β,γ)

instance ∀ α β γ δ . TupleAppend (α,β,γ) δ (α,β,γ,δ) where
  type instance TupleAppended (α,β,γ) δ = (α,β,γ,δ)
  tupleAppend (α,β,γ) δ = (α,β,γ,δ)

instance ∀ α β γ δ κ . TupleAppend (α,β,γ,δ) κ (α,β,γ,δ,κ) where
  type instance TupleAppended (α,β,γ,δ) κ = (α,β,γ,δ,κ)
  tupleAppend (α,β,γ,δ) κ = (α,β,γ,δ,κ)

instance ∀ α β γ δ κ ι . TupleAppend (α,β,γ,δ,κ) ι (α,β,γ,δ,κ,ι) where
  type instance TupleAppended (α,β,γ,δ,κ) ι = (α,β,γ,δ,κ,ι)
  tupleAppend (α,β,γ,δ,κ) ι = (α,β,γ,δ,κ,ι)

