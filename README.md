
<img src="https://raw.githubusercontent.com/SAP/bayesian-network-builder/master/docs/logo.png" width="100"> Bayesian Network Builder 
=====

[![REUSE status](https://api.reuse.software/badge/github.com/SAP/bayesian-network-builder)](https://api.reuse.software/info/github.com/SAP/bayesian-network-builder)

A [Bayesian Belief Network](https://en.wikipedia.org/wiki/Bayesian_network) (BBN), or simply Bayesian Network, is a statistical model used to describe the [conditional dependencies](https://en.wikipedia.org/wiki/Conditional_dependence) between different random variables.

# Random Variables
```scala
import com.sap.bnb.bn._
val bool = Flip(.6)
println(bool)
```

```
false->.40, true->.60
```

```scala
import com.sap.bnb.bn._
val categorical = Cards("male" -> .5,"female" -> .5)

```

# Burglary Example
I'm at work, neighbor John calls to say my alarm is ringing, but
neighbor Mary doesn't call. Sometimes the alarm is set off by
minor earthquakes. Both burglary and earthquake are rather rare events.

The question we want to answer: _**Is there really a burglar at home?**_

Considering that:

- John always calls when he hears the alarm, but sometimes
  confuses the telephone ringing with the alarm.

- Mary likes rather loud music and sometimes misses the alarm. 

![Burglary](docs/alarm.png)
```scala
import com.sap.bnb.bn._
import com.sap.bnb.dsl._

val g = graph {
 "burglar" <~ Flip(.001)
 "earthquake" <~ Flip(.002)
 "alarm" <~ ("burglar", "earthquake",
     (true, true) -> Flip(.95),
     (true, false) -> Flip(.94),
     (false, true) -> Flip(.29),
     (false, false) -> Flip(.001))
  "alarm" ~ (true -> Flip(.9), false -> Flip(.05)) ~> "JohnCalls"
  "alarm" ~ (true -> Flip(.7), false -> Flip(.01)) ~> "MaryCalls"
}
val burglar = g.evidences("JohnCalls" -> true, "MaryCalls" -> false)
      .solve("burglar").value.get
println(s"posterior: $burglar")
println("chances burglary: " + f"${burglar.chances(true) * 100}%2.1f%%")
```

```
posterior: true -> .005, false -> .995
chances burglary: 0,6%
```

# Dynamic Bayesian Networks
A Dynamic Bayesian Network (DBN) is a Bayesian network (BN) which relates variables to each other over adjacent time steps. 
![](docs/dbn.png)
```scala
import com.sap.bnb.bn._
import com.sap.bnb.dsl._
val g = graph {
  "highPressure" ~ (true -> Flip(.9), false -> Flip(.2)) ~> "sunny"
  "sunny" ~ (true -> Flip(.05), false -> Flip(.8)) ~> "highHumidity"
  "highHumidity" ~ (true -> Flip(.2), false -> Flip(.9)) ~~> "highPressure"
}
val w1 = g.evidences("highPressure" -> true).solve[Boolean]("sunny")
println("sunny day 1: " + f"${w1.value.get.chances(true) * 100}#2.1f%%") 
val w2 = w1.next.solve[Boolean]("sunny")
println("sunny day 2: " + f"${w2.value.get.chances(true) * 100}#2.1f%%") 
```
```
sunny day 1: 89%
sunny day 2: 76%
```

## Get Started
Install [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html).

```sbt
sbt test publishLocal
```

then in your sbt project add 
```sbt
libraryDependencies += "com.sap" % "bnb" % "0.1"
```

## Contribute

Contributions are welcome!
If you're interested please check these two important documents:

* [CONTRIBUTING.md](CONTRIBUTING.md) contains operational details on how to contribute and explains how to engage with the community. If you are new to this project and want to contribute, please start here.

## Contact

Contact me under [Giancarlo Frison](https://gfrison.com)

## Licensing 

Copyright 2020-2021 SAP SE or an SAP affiliate company and bayesian-network-builder contributors. Please see our [LICENSE](LICENSE) for copyright and license information. Detailed information including third-party components and their licensing/copyright information is available [via the REUSE tool](https://api.reuse.software/info/github.com/SAP/bayesian-network-builder).
