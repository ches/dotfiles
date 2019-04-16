name         := "project-name"
organization := "net.whiskeyandgrits"
version      := "0.1-SNAPSHOT"
homepage     := Some(url("https://github.com/ches/project-name"))

scalaVersion       := "2.12.8"
crossScalaVersions := Seq("2.11.12", scalaVersion.value)
scalacOptions     ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint")

resolvers ++= Seq(
  // Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= {
  val akkaVersion     = "2.5.22"
  val akkaHttpVersion = "10.1.8"
  Seq (
    "com.typesafe"               %  "config"                   % "1.3.3"
    // -- Akka --
  , "com.typesafe.akka"          %% "akka-actor"               % akkaVersion
  , "com.typesafe.akka"          %% "akka-remote"              % akkaVersion
  , "com.typesafe.akka"          %% "akka-slf4j"               % akkaVersion
  , "com.typesafe.akka"          %% "akka-stream"              % akkaVersion
  , "com.typesafe.akka"          %% "akka-multi-node-testkit"  % akkaVersion     % Test
  , "com.typesafe.akka"          %% "akka-stream-testkit"      % akkaVersion     % Test
  , "com.typesafe.akka"          %% "akka-testkit"             % akkaVersion     % Test
    // -- Akka HTTP --
  , "com.typesafe.akka"          %% "akka-http"                % akkaHttpVersion
  , "com.typesafe.akka"          %% "akka-http-spray-json"     % akkaHttpVersion
  , "com.typesafe.akka"          %% "akka-http-testkit"        % akkaHttpVersion % Test
    // -- Logging --
  , "ch.qos.logback"             %  "logback-classic"          % "1.2.3"
  , "com.typesafe.scala-logging" %% "scala-logging"            % "3.9.2"
    // -- Testing --
  , "org.scalatest"              %% "scalatest"                % "3.0.5"         % Test
  , "org.scalacheck"             %% "scalacheck"               % "1.14.0"        % Test
  )
}

// console convenience, but skip in quick in case we've broken the build!
initialCommands in console := "import my.package._"
initialCommands in consoleQuick := ""
