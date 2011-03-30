import sbt._

class HelloWorldProject(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions: Seq[CompileOption] = Optimize :: Deprecation :: Nil
  override def dependencyPath = "lib"
}