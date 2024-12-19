
template width*(this: SomeInteger, width: int): SomeInteger =
  (this and (static((1.shl width) - 1)))