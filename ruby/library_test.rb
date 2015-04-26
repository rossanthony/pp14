# File:  library_test.rb
 
require_relative "library"
require "test/unit"
 
class LibraryTester < Test::Unit::TestCase
 
  def setup
    @lib = Library.new
  end

  def test_initialize_makes_calendar
    assert_nothing_raised { @lib.calendar.get_date }
  end


  def test_load_all_books
    assert @lib.books.size == 7, 'Did not load all books in collection'
  end


  def teardown
    ## Nothing really
  end
 
end