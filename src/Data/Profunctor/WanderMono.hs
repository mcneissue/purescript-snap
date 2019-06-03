class WanderMono p where
  wanderMono :: forall a s. (forall f. Applicative f => (a -> f a) -> (s -> f s)) -> p a a -> p s s
