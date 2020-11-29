# My own iterator tools lib, inspired by nimLazy

proc skip*[T](iter: iterator():T, n: Natural): iterator():T =
  for i in 0..n:
    discard iter()
  result = iterator(): T =
    for c in iter:
      yield c
