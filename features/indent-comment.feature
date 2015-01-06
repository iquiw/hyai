Feature: Indent comment
  In order to write comment efficiently
  An Emacs user will need not to indent comments

  Scenario: Before comment
    Given the buffer is empty
    When I insert:
    """
    {-# LANGUAGE OverloadedStrings #-}
    {-
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "()"

    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          --
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "()"
