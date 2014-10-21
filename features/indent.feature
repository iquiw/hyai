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
      when
        bar = do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"
