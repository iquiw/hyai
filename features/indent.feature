Feature: hyai indent
  In order to code Haskell faster
  As an Emacs user
  I want to indent code automatically

  Scenario: After do
    Given the buffer is empty
    When I insert:
    """
    main = do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar
      where
        bar = do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

  Scenario: After where
    Given the buffer is empty
    When I insert:
    """
    module Main where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar
      where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar
      where
        bar = baz
          where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

  Scenario: Before where
    Given the buffer is empty
    When I insert:
    """
    foo = bar
    where
    """
    And I place the cursor before "where"
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(2)"

    Given the buffer is empty
    When I insert:
    """
    foo = do
        bar
    where
    """
    And I place the cursor before "where"
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(2)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar
      where
        bar = baz
    where
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6)"

  Scenario: After normal line
    Given the buffer is empty
    When I insert:
    """
    main = do
        foo bar
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 8 0)"
