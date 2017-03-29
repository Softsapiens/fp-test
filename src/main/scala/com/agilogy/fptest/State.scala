package com.agilogy.fptest

object State {
  type State[S,T] = S => (S,T)


  //TODO: How do they define a state monad?
  //  implicit def stateMonad[S,T]:Monad[State[S,T]] = new Monad[State[S,T]] {
  //    override def map[T, T2](m: State[S,T])(f: (T) => T2): State[T2] = ???
  //
  //    override def flatMap[T, T2](m: State[T])(f: (T) => State[T2]): State[T2] = ???
  //
  //    override def pure[T](v: T): State[T] = ???
  //  }

}