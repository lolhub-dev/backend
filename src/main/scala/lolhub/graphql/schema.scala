import sangria.schema._

object SchemaDefinition {

val Query = ObjectType[Hello, Unit](
  "Query", fields[Hello, Unit](
      Field("name", StringType,
        Some("teststring"),
        resolve = "test"),
  ))

val Schema = Schema(Query)

}