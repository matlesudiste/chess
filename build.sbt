Test / parallelExecution := false
Test / logBuffered := false

resolvers += "Sonatype OSS Snapshots" at
	"https://oss.sonatype.org/content/repositories/snapshots"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

lazy val root = (project in file("."))
	.settings(
		name := "Chess engine by Mathias Payan",
		libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
		libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.18",
		libraryDependencies += "com.github.blemale" %% "scaffeine" % "3.1.0" % "compile"
	)
