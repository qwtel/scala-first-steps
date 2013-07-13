package bobsrockets {
  package navigation {
    class Navigator
  }

  package lunch {
    class Booster {
      // No need to say bobsrockets.navigation.Navigator
      val nav = new navigation.Navigator
    }
  }

  package tests {
    class NavigatorSuite
  }
}
