package imem

trait OwnerCarrier:
end OwnerCarrier

/** TODO: make it private, and only pass it in a scope: e.g. `withOwnership(body...)`
  */
class DefaultOwnerCarrier extends OwnerCarrier:
end DefaultOwnerCarrier
