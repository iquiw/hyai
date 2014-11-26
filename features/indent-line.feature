Feature: hyai indent line
  In order to choose indent candidates
  As an Emacs user
  I want to change indent by tab press

  Scenario: retain cursor position
    Given the buffer is empty
    When I insert:
    """
    main = do
    foobar
    """
    And I go to beginning of line
    And I press "<tab>"
    Then current indentation is 4
    Then current column is 4

    Given the buffer is empty
    When I insert:
    """
    main = do
    foobar
    """
    And I place the cursor after "foo"
    And I press "<tab>"
    Then current indentation is 4
    Then current column is 7

    Given the buffer is empty
    When I insert:
    """
    main = do
    foobar
    """
    And I place the cursor after "foobar"
    And I press "<tab>"
    Then current indentation is 4
    Then current column is 10

  Scenario: move cursor position at the beginning
    Given the buffer is empty
    When I insert:
    """
    main = do
             foobar
    """
    And I go to beginning of line
    And I press "<tab>"
    Then current indentation is 4
    Then current column is 4
