# File:  library_test.rb
 
require_relative "library"
require "test/unit"
 
class TestLibrary < Test::Unit::TestCase
 
  def setup
    lib = Library.new
    lib.open
    #lib.search('saga')
    lib.issue_card('ross')
    lib.serve('ross')
    lib.check_out(1,2,3)

    # Advance by 1 week
    lib.close
    lib.open
    lib.close
    lib.open
    lib.close
    lib.open
    lib.close
    lib.open
    lib.close
    lib.open
    lib.close
    lib.open
    lib.close
    lib.open
    lib.close
    lib.open

    lib.find_overdue_books
    lib.find_all_overdue_books
    lib.check_in(3)
    lib.renew(1)
    lib.find_all_overdue_books
  end
 
  def teardown
    ## Nothing really
  end
 
end