# File:  library_test.rb
 
require_relative "library"
require "test/unit"
 
class LibraryTester < Test::Unit::TestCase
 
  def setup
    @lib = Library.new
  end

  def test_library_open
    assert @lib.open == 'Today is day 1'
  end

  def test_library_issue_card
    @lib.open
    assert @lib.issue_card('ross') == 'Library card issued to ross.'
  end

  def test_library_serve
    @lib.open
    @lib.issue_card('ross')
    assert @lib.serve('ross') == 'Now serving ross.'
  end

  def test_library_serve_unknown_user
    @lib.open
    assert @lib.serve('nobody') == 'nobody does not have a library card.'
  end

  def test_library_serve_not_open
    assert_raise("The library is not open.") { @lib.serve('ross') }
  end

  def test_library_search
    @lib.open
    assert @lib.search('saga') == "1: Contact, by Carl Sagan\n3: Cosmos, by Carl Sagan\n4: Pale Blue Dot, by Carl Sagan\n"
  end

  def test_library_search_less_than_four
    @lib.open
    assert @lib.search('abc') == 'Search string must contain at least four characters.'
  end

  def test_library_search_no_books_found
    @lib.open
    assert @lib.search('abcde') == 'No books found.'
  end

  def test_library_search_not_open
    assert_raise("The library is not open.") { @lib.serve('ross') }
  end

  def test_find_overdue_books
    @lib.open
    @lib.issue_card('ross')
    @lib.serve('ross')
    @lib.check_out(1)
    @lib.close
    @lib.open
    @lib.close
    @lib.open
    @lib.close
    @lib.open
    @lib.close
    @lib.open
    @lib.close
    @lib.open
    @lib.close
    @lib.open
    @lib.close
    @lib.open
    @lib.close
    @lib.open
    assert @lib.find_overdue_books() == "1: Contact, by Carl Sagan\n"
  end

  def test_find_overdue_books_nothing_overdue
    @lib.open
    @lib.issue_card('ross')
    @lib.serve('ross')
    @lib.check_out(1)
    assert @lib.find_overdue_books() == "None."
  end

  def test_find_overdue_books_not_open
    assert_raise("The library is not open.") { @lib.find_overdue_books() }
  end

  def test_find_overdue_books_not_open
    @lib.open
    assert_raise("No member is currently being served.") { @lib.find_overdue_books() }
  end  

  def test_check_in_not_open
    assert_raise("The library is not open.") { @lib.check_in(1) }
  end  

  def test_check_in_no_member_being_served
    @lib.open
    assert_raise("No member is currently being served.") { @lib.check_in(1) }
  end  

  def test_check_in_no_books_ids
    @lib.open
    @lib.issue_card('ross')
    @lib.serve('ross')
    assert_raise("Please specifiy at least one book ID.") { @lib.check_in() }
  end

  def test_check_in_returning_one_book
    @lib.open
    @lib.issue_card('ross')
    @lib.serve('ross')
    @lib.check_out(1)
    assert @lib.check_in(1) == "ross has returned 1 books."
  end

  def test_check_in_returning_two_books
    @lib.open
    @lib.issue_card('ross')
    @lib.serve('ross')
    @lib.check_out(1,4)
    assert @lib.check_in(1,4) == "ross has returned 2 books."
  end

  def test_check_out_not_open
    assert_raise("The library is not open.") { @lib.check_out(1) }
  end

  def test_check_out_no_member_being_served
    @lib.open
    assert_raise("No member is currently being served.") { @lib.check_out(1) }
  end  

  def test_check_out_no_books_ids
    @lib.open
    @lib.issue_card('ross')
    @lib.serve('ross')
    assert_raise("Please specifiy at least one book ID.") { @lib.check_out() }
  end

  def test_check_out_one_book
    @lib.open
    @lib.issue_card('ross')
    @lib.serve('ross')
    assert @lib.check_out(1) == "1 books have been checked out to ross."
  end

  def test_check_out_two_books
    @lib.open
    @lib.issue_card('ross')
    @lib.serve('ross')
    assert @lib.check_out(1,4) == "2 books have been checked out to ross."
  end

  def test_close_when_not_open
    assert_raise() { @lib.close }
  end

  def test_close_when_open
    @lib.open
    assert @lib.close == "Good night."
  end

end