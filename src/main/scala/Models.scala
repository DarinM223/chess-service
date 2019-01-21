import cats.implicits._
import cats.data.OptionT
import cats.effect.{IO, Sync}
import doobie.implicits._
import doobie.util.transactor.Transactor
import tsec.authentication.BackingStore
import scala.collection.mutable

object Models {
  case class User(id: Int, name: String, hashedPassword: String)

  def dummyBackingStore[F[_], I, V](getId: V => I)(implicit F: Sync[F]) = new BackingStore[F, I, V] {
    private val storageMap = mutable.HashMap.empty[I, V]

    def put(elem: V): F[V] = {
      val map = storageMap.put(getId(elem), elem)
      if (map.isEmpty)
        F.pure(elem)
      else
        F.raiseError(new IllegalArgumentException)
    }

    def get(id: I): OptionT[F, V] =
      OptionT.fromOption[F](storageMap.get(id))

    def update(v: V): F[V] = {
      storageMap.update(getId(v), v)
      F.pure(v)
    }

    def delete(id: I): F[Unit] =
      storageMap.remove(id) match {
        case Some(_) => F.unit
        case None    => F.raiseError(new IllegalArgumentException)
      }
  }

  def doobieBackingStore(xa: Transactor[IO]) = new BackingStore[IO, Int, User] {
    override def put(user: User): IO[User] =
      sql"""
          INSERT INTO users (name, password) VALUES (${user.name}, ${user.hashedPassword})
        """.update.run.transact(xa).as(user)

    override def get(id: Int): OptionT[IO, User] = {
      val query =
        sql"""
            SELECT id, name FROM users WHERE id = $id
        """.query[User].option.transact(xa)
      OptionT(query)
    }

    override def update(user: User): IO[User] =
      sql"""
            UPDATE users SET name = ${user.name} WHERE id = ${user.id}
        """.update.run.transact(xa).as(user)

    override def delete(id: Int): IO[Unit] =
      sql"""
            DELETE FROM users where id = $id
        """.update.run.transact(xa).as(())
  }
}
