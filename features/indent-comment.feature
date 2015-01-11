Feature: Indent comment
  In order to write comment efficiently
  An Emacs user will need not to indent comments

  Scenario: Comment start
    Given the buffer is empty
    When I insert:
    """
    {-# LANGUAGE OverloadedStrings #-}
    """
    And I place the cursor before "{-"
    And I press "<tab>"
    Then current indentation is 0
    Then current column is 0

    Given the buffer is empty
    When I insert:
    """
    {-# LANGUAGE OverloadedStrings #-}
    {-# G
    """
    And I place the cursor before "{-# G"
    And I press "<tab>"
    Then current indentation is 0
    Then current column is 0

    Given the buffer is empty
    When I insert:
    """
      {-
    """
    And I place the cursor before "  {-"
    And I press "<tab>"
    Then current indentation is 2
    Then current column is 2

    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          -- bar
    """
    And I place the cursor before " --"
    And I press "<tab>"
    Then current indentation is 6
    Then current column is 6

  Scenario: Comment end
    Given the buffer is empty
    When I insert:
    """
       {-
          foo = bar
    -}
    """
    And I press "<tab>"
    Then current indentation is 3
    Then current column is 5

    Given the buffer is empty
    When I insert:
    """
      {-
          foo = bar
           -}
    """
    And I place the cursor before "  -}"
    And I press "<tab>"
    Then current indentation is 2
    Then current column is 2

  Scenario: In nestable comment
    Given the buffer is empty
    When I insert:
    """
    {- foo
    """
    And I press "<tab>"
    Then current indentation is 0
    Then current column is 6

    Given the buffer is empty
    When I insert:
    """
    {-
      main = do
    
    """
    And I press "<tab>"
    Then current indentation is 2
    # This is behavior of indent-relative.
    And I press "<tab>"
    Then current indentation is 7

  Scenario: In one line comment
    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          -- bar
    """
    And I place the cursor before "bar"
    And I press "<tab>"
    Then current indentation is 6
    Then current column is 9
