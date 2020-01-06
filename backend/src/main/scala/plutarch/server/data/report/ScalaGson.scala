package plutarch.server.data.report

import java.lang.reflect.Type
import com.google.gson._
import scala.collection.JavaConverters._

object ScalaGson {

  lazy val gson: Gson = new GsonBuilder()
    .registerTypeHierarchyAdapter(classOf[Seq[_]], new ListSerializer)
    .registerTypeHierarchyAdapter(classOf[Map[_, _]], new MapSerializer)
    .serializeNulls()
    .create()

  lazy val prettyGson: Gson = gson.newBuilder().setPrettyPrinting().create()

  class ListSerializer extends JsonSerializer[Seq[_]] {
    def serialize(src: Seq[_], typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
      context.serialize(src.toList.asJava)
    }
  }

  class MapSerializer extends JsonSerializer[Map[_, _]] {
    def serialize(src: Map[_, _], typeOfSrc: Type, context: JsonSerializationContext): JsonElement = {
      context.serialize(src.asJava)
    }
  }

}