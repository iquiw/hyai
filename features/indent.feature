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
    module Foo
        ( foo
        , bar
        , baz
        ) where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

    Given the buffer is empty
    When I insert:
    """
    class Foo a where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

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

  Scenario: After case
    Given the buffer is empty
    When I insert:
    """
    foobar = case baz of
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 13)"

  Scenario: Before then
    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo
    then
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

  Scenario: Before else
    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo
            then
    else
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

  Scenario: After open parenthesis
    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6)"

  Scenario: Before open parenthesis
    Given the buffer is empty
    When I insert:
    """
    module Foo
    (
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: Before closed parenthesis
    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          foo
        , bar
        , baz
    )
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          foo
        , (<^^>)
        , bar
    )
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: After colon
    Given the buffer is empty
    When I insert:
    """
    import Data.Text (foo,
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(18)"

  Scenario: Before colon
    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          foo
    ,
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    import Data.Text ( foo
                     , bar
    ,
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(17)"

  Scenario: Before ->
    Given the buffer is empty
    When I insert:
    """
    foobar :: ByteString
    ->
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(7)"

    Given the buffer is empty
    When I insert:
    """
    foobar :: ByteString
           -> (Char -> Char)
    ->
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(7)"

  Scenario: After equal line
    Given the buffer is empty
    When I insert:
    """
    main = foobar
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(7 0)"

  Scenario: After normal line
    Given the buffer is empty
    When I insert:
    """
    main = do
        foobar
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 8 0)"
