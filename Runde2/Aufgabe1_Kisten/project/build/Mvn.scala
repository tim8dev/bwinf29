import sbt._

class FooProject(info: ProjectInfo) extends DefaultProject(info) {

val mavenLocal = "Local Maven Repository" at
"file://"+Path.userHome+"/.m2/repository"
}