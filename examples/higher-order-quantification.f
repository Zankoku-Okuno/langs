Λf:* -> *.Λa:*.Λb:*. λ(map:∀x:*.∀y:*.(x -> y) -> f x -> f y).
    λ(f:a -> b).λ(xs:f a). map[a][b] f xs