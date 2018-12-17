object dependencies {

  import sbt._

  object cats {
    val VERSION       = "1.4.0"
    val mtl           = "org.typelevel"     %% "cats-mtl-core"        % "0.4.0"
    val core          = "org.typelevel"     %% "cats-core"            % VERSION
    val laws          = "org.typelevel"     %% "cats-laws"            % VERSION
    val scalacheck    = "io.chrisdavenport" %% "cats-scalacheck"      % "0.1.0" excludeAll(
      ExclusionRule(organization = "org.scalacheck") // scalacheck 1.14 is incompatible with scalatest
      )
    val testkit       = "org.typelevel"     %% "cats-testkit"         % VERSION

    object effect {
      val VERSION     = "1.0.0"

      val core        = "org.typelevel"     %% "cats-effect"          % VERSION
      val laws        = "org.typelevel"     %% "cats-effect-laws"     % VERSION
    }
  }


  object fs2 {
    val VERSION       = "1.0.0"
    val core          = "co.fs2"            %% "fs2-core"             % VERSION
    val io            = "co.fs2"            %% "fs2-io"               % VERSION
  }

  val scalatest       = "org.scalatest"     %% "scalatest"            % "3.0.5"


}
