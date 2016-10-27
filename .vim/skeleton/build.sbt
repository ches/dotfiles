name         := "project-name"
organization := "net.whiskeyandgrits"
version      := "0.1-SNAPSHOT"
homepage     := Some(url("https://github.com/ches/project-name"))

scalaVersion       := "2.11.8"
crossScalaVersions := Seq("2.10.6", scalaVersion.value)
scalacOptions     ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")

resolvers ++= Seq(
  // Resolver.typesafeRepo("releases")
  // Resolver.sonatypeRepo("snapshots")
  // "Spray Repository" at "http://repo.spray.io"
)

libraryDependencies ++= {
  val akkaVersion  = "2.3.7"
  val sprayVersion = "1.3.2"
  Seq (
    "com.typesafe"               %  "config"                   % "1.2.1"
    // -- Akka --
  , "com.typesafe.akka"          %% "akka-actor"               % akkaVersion
  , "com.typesafe.akka"          %% "akka-remote"              % akkaVersion
  , "com.typesafe.akka"          %% "akka-slf4j"               % akkaVersion
  , "com.typesafe.akka"          %% "akka-multi-node-testkit"  % akkaVersion   % "test"
  , "com.typesafe.akka"          %% "akka-testkit"             % akkaVersion   % "test"
    // -- Spray --
  , "io.spray"                   %% "spray-can"                % sprayVersion
  , "io.spray"                   %% "spray-client"             % sprayVersion
  , "io.spray"                   %% "spray-routing"            % sprayVersion
  , "io.spray"                   %% "spray-testkit"            % sprayVersion  % "test"
    // -- Logging --
  , "ch.qos.logback"             %  "logback-classic"          % "1.1.2"
  , "com.typesafe.scala-logging" %% "scala-logging"            % "3.1.0"
    // -- Testing --
  , "org.scalatest"              %% "scalatest"                % "2.2.2"       % "test"
  , "org.scalacheck"             %% "scalacheck"               % "1.12.2"      % "test"
  )
}

// console convenience, but skip in quick in case we've broken the build!
initialCommands in console := "import my.package._"
initialCommands in consoleQuick := ""
