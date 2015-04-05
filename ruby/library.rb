# library.rb  
#
#
# @author Ross Anthony

require 'set'

# class Calendar  
#
#   We need to deal with the passage of time, so we need to keep track of
#   dates. Ruby has a perfectly good Time class, but to keep things simple,
#   we will just use an integer to keep track of how many days have passed.

class Calendar  

  def initialize()  
    # Instance variables
    @time = 0  
  end  
  
  # Returns (as an integer) the current date.  
  def get_date()
    @time
  end

  # Increment the date (move ahead to the next day), and returns the new date.
  def advance()
    @time = @time + 1
  end
  
end  

# class Book  
#
#   A book has these attributes (instance variables): id, title, author
#   (only one author per book), and due_date. The due date is nil if the
#   book is not checked out.
class Book

  def initialize(id, title, author)
    @id = id
    @title = title
    @author = author
    @due_date = nil
  end

  # Returns this book's unique identification number.
  def get_id()
    return @id
  end

  # Returns this book's title.
  def get_title()
    return @title
  end

  # Returns this book's author.
  def get_author()
    return @author
  end

  # Returns the date (as an integer) that this book is due.  
  def get_due_date()
    return @due_date
  end

  # Sets the due date of this Book. Doesn't return anything.
  def check_out(due_date)
    @due_date = due_date
  end
  
  # Sets the due date of this Book to nil. Doesn't return anything.
  def check_in()
    @due_date = nil
  end

  # Returns a string of the form "id: title, by author”.
  def to_s()
    return @id.to_s + ": " + @title + ", by " + @author
  end
  
end

# class Member
#    
#    A member is a "customer" of the library. A member must have a library
#    card in order to check out books. A member with a card may have no
#    more than three books checked out at any time.

class Member

  # Constructs a member with the given name, and no books. The member 
  # must also have a reference to the Library object that he/she uses.
  def initialize(name, library)
    @name = name
    @library = library
    @checked_books = Set.new
  end

  # Returns this member's name.
  def get_name()
    @name
  end

  # Adds this Book object to the set of books checked out by this member.
  def check_out(book)
    if @checked_books.count < 3
      @checked_books.add book
      true
    else
      puts "Could not add book id: #{book.get_id}, max no of books (3) reached."
      false
    end
  end

  # Removes this Book object from the set of books checked out by this member. 
  # (Since members are usually said to "return" books, this method should be called return !)   
  def return(book)
    @checked_books.delete?(book)
  end

  # # Returns the set of Book objects checked out to this member (may be the empty set).
  def get_books()
    @checked_books
  end

  # Tells this member that he/she has overdue books. (What the method actually 
  # does is just print out this member's name along with the notice.)
  def send_overdue_notice(notice)
    puts "#{@name} #{notice}"
  end
end


# class Library
#    
#    This is the "master" class. It has a number of methods which are called
#    by the "librarian" (the user), not by members. The librarian does all the
#    work. Since these methods (other than the constructor) will be typed in
#    by the librarian, all the parameters must be strings or numbers, not
#    objects. Furthermore, to reassure the librarian that his/her action has
#    worked, all these methods should return a result, for example, "Library
#    card issued to Lenny Bruce.”

class Library

  # Constructs the Library object (you should create exactly one of these).
  def initialize()
    @books = {}
    # Read in, from a file named collection.txt, a list of (title,author) tuples, 
    collection = File.new('collection.txt')
    
    i = 1
    collection.each do |row|
      book = row.chomp.split(', ')
      unless book[0].empty?
        # Create a Book from each tuple
        @books[i] = Book.new(i, book[0], book[1])
        i = i + 1
      end
    end
    
    # Create a Calendar object (you should create exactly one of these).
    @cal = Calendar.new

    # Define an empty dictionary of members. 
    #  - The keys will be the names of members and the values will be the corresponding Member objects.
    @members = {
      'ross' =>  Member.new('ross', self), 
      'keith' => Member.new('keith', self), 
      'trevor' => Member.new('trevor', self)
    }

    # Set a flag variable to indicate that the library is not open.
    @open = nil
    # Set the current member (the one being served) to nil
    @current_member = nil

    @library_cards = {}
  end

  # If the library is already open, raises an Exception with the message "The library is already open!".
  # Otherwise, starts the day by advancing the Calendar, and setting the flag to indicate that the
  # library is open. (). Returns: The string "Today is day n.”
  def open()
    if @open
      puts "The library is already open!"
    else
      @open = true
      @cal.advance
      puts "Today is day " + @cal.get_date.to_s
    end
  end

  # Prints a nicely formatted, multiline string, listing the names of members who have overdue books, and for each such member, the
  # books that are overdue. Or, the string "No books are overdue.”.
  def find_all_overdue_books()
    # loop through the members and check if they have any overdue books, then print them out
    @members.each do |k,member| 
      overdue_books = member.get_books.select { |book| 
        @cal.get_date > book.get_due_date()
      }
      if overdue_books.count > 0
        puts "#{member.get_name} has the following overdue book(s)..."
        overdue_books.each do |book|
          puts "  #{book.to_s}"
        end
      else
        puts "#{member.get_name} has no books overdue."
      end

    end
  end

  # Issues a library card to the person with this name. However, no member should be permitted to have more than one library card.
  # Returns either "Library card issued to name_of_member." or "name_of_member already has a library card.”.
  # Possible Exception: "The library is not open.".
  def issue_card(name_of_member)
    if not @open
      puts "The library is not open."
    else
      # check if they already have a card?
      if @library_cards.has_key?(name_of_member.to_sym) 
        puts "#{name_of_member} already has a library card."
      elsif @members.has_key?(name_of_member.to_sym) 
        puts "#{name_of_member} is not a member."
      else
        @library_cards[name_of_member.to_sym] = nil
        puts "Library card issued to #{name_of_member}."
      end
    end
  end

  # Specifies which member is about to be served (and quits serving the previous member, if any). 
  # The purpose of this method is so that you don't have to type in the person's name again and again 
  # for every book that is to be checked in or checked out. What the method should actually do is to 
  # look up the member's name in the dictionary, and save the returned Member object in a data variable 
  # of this library. Returns either "Now serving name_of_member." 
  # or "name_of_member does not have a library card.". 
  # Possible Exception: "The library is not open."
  def serve(name_of_member)
    if not @open
      puts "The library is not open."
    else
      # lookup the member
      if @current_member == name_of_member
        puts "Already serving #{name_of_member}."
      elsif @library_cards.has_key?(name_of_member.to_sym)
        @current_member = name_of_member
        puts "Now serving #{name_of_member}."
      else
        puts "#{name_of_member} does not have a library card."
      end
    end
  end

  # Prints a multiline string, each line containing one book (as returned by the book's to_s method), 
  # of the books that have been checked out by the member currently being served, and which are overdue. 
  # If the member has no overdue books, the string “None” is printed.
  # May throw an Exception with an appropriate message: 
  #  - "The library is not open.”
  #  - "No member is currently being served.”
  def find_overdue_books()
    if not @open
      puts "The library is not open."
    elsif not @current_member
      puts "No member is currently being served."
    else
      overdue_books = @members["#{@current_member}"].get_books.select { |book| 
        @cal.get_date > book.get_due_date()
      }
      if overdue_books.count > 0
        overdue_books.each do |book|
          puts book.to_s
        end
      else
        puts "None."
      end
    end
  end

  # The book is being returned by the current member (there must be one!), so return it 
  # to the collection and remove it from the set of books currently checked out to the member. 
  # The book_numbers are taken from the list printed by the search command. Checking in a
  # Book will involve both telling the Book that it is checked in and returning the Book to this 
  # library's collection of available Books. If successful, returns "name_of_member has returned n books.”.
  # May throw an Exception with an appropriate message:
  # - "The library is not open."
  # - "No member is currently being served."
  # - "The member does not have book id.”
  def check_in(*book_ids) # * = 1..n of book_ids
    if not @open
      puts "The library is not open."
    elsif not @current_member
      puts "No member is currently being served."
    elsif not book_ids.all? {|i| i.is_a? Fixnum }
      puts "Please specifiy at least one book ID."
    else
      total = 0
      # lookup the books, check they exist and are checked out 
      book_ids.each do |id|
        @books.select { |k,book| 
          !book.get_due_date().nil? && book.get_id() == id.to_i 
        }.to_a.each do |k,book| 
          if @members["#{@current_member}"].return(book)
            book.check_in
            total = total + 1
          else
            # 
            puts "The member does not have book id #{id}."
          end
        end
      end
      if total > 0
        puts "#{@current_member} has returned #{total.to_s} books."
      else
        puts "No books found."
      end
    end
  end

  # Finds those Books whose title or author (or both) contains this string. For example, the 
  # string "tact" might return, among other things, the book Contact, by Carl Sagan. The search 
  # should be caseinsensitive; that is, "saga" would also return this book. Only books which are 
  # currently available (not checked out) will be found. If there is more than one copy of a book 
  # (with the same title and same author), only one will be found. In addition, to keep from
  # returning too many books, require that the search string be at least 4 characters long.
  # Returns one of:
  # - "No books found."
  # - "Search string must contain at least four characters."
  # - A multiline string, each line containing one book (as returned by the book's to_s method.)
  def search(string)
    if not @open
      puts "The library is not open."
    elsif string.empty? || string.length < 4
      puts "Search string must contain at least four characters."
    else
      results = @books.select { |k,book| 
        (book.get_title().downcase.include?(string.downcase) || book.get_author().downcase.include?(string.downcase)) && book.get_due_date() == nil
      }
      if results.count > 0
        results.to_a.uniq { |k,v| 
          v.get_author() && v.get_title() 
        }.each { |k,v| puts v.to_s }
      else
        puts "No books found."
      end
    end
  end

  # Checks out the book(s) to the member currently being served (there must be one!), or tells why 
  # the operation is not permitted. The book_ids could have been found by a recent call to the 
  # search method. Checking out a book will involve both telling the book that it is checked out 
  # and removing the book from this library's collection of available books.
  # If successful, returns "n books have been checked out to name_of_member.".
  # May throw an Exception with an appropriate message:
  # - "The library is not open." 
  # - "No member is currently being served." 
  # - "The library does not have book id."
  def check_out(*book_ids) # 1..n book_ids
    if not @open
      puts "The library is not open."
    elsif not @current_member
      puts "No member is currently being served."
    elsif not book_ids.all? {|i| i.is_a? Fixnum }
      puts "Please specifiy at least one book ID."
    elsif @members["#{@current_member}"].get_books.count > 3
      puts "#{@current_member} has checked out the max number of books."
    else
      total = 0
      # lookup the books, check they exist and aren't already checked out 
      book_ids.each do |v|
        @books.select { |k,book| 
          book.get_due_date() == nil && book.get_id() == v.to_i 
        }.to_a.each do |k,book| 
          if @members["#{@current_member}"].check_out(book)
            book.check_out(@cal.get_date + 7)
            total = total + 1
          end
        end
      end
      if total > 0
        puts "#{total.to_s} books have been checked out to #{@current_member}."
      else
        puts "No books found."
      end
    end
  end

  # Renews the books for the member currently being served (by setting their due dates to today's 
  # date plus 7) or tells why the operation is not permitted. 
  # If successful, returns "n books have been renewed for name_of_member.".
  # May throw an Exception with an appropriate message:
  # - "The library is not open." 
  # - "No member is currently being served." 
  # - "The member does not have book id."
  def renew(*book_ids) # 1..n book_ids
    if not @open
      puts "The library is not open."
    elsif not @current_member
      puts "No member is currently being served."
    elsif not book_ids.all? {|i| i.is_a? Fixnum }
      puts "Please specifiy at least one book ID."
    else
      total = 0
      # lookup the books, check they exist and are checked out 
      book_ids.each do |id|
        @books.select { |k,book| 
          !book.get_due_date().nil? && book.get_id() == id.to_i 
        }.to_a.each do |k,book| 
          if @members["#{@current_member}"].get_books.member?(book)
            book.check_out(@cal.get_date + 7)
            total = total + 1
          end
        end
      end
      if total > 0
        puts "#{total.to_s} books have been renewed for #{@current_member}."
      else
        puts "The member does not have book id(s)."
      end
    end
  end

  # Shut down operations and go home for the night. None of the other operations (except quit) can be used 
  # when the library is closed. If successful, returns the string "Good night.".
  # May throw an Exception with the message: "The library is not open.”
  def close()
    if @open
      @open = false
      puts "Good night."
    else
      puts "The library is not open."
    end
  end

  # The mayor, citing a budget crisis, has stopped all funding for the library. Can happen at any time. 
  # Returns the string "The library is now closed for renovations.”
  def quit()
    puts "The library is now closed for renovations."
  end
end

class Test 
  def initialize()
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
end
