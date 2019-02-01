package com.d_m.Models

import doobie.util.transactor.Transactor

trait GameRepository[F[_]] {
}

class GameRepositoryImpl[F[_]](xa: Transactor[F]) extends GameRepository[F] {
}
