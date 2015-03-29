# library.rb  

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
    puts @time
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
  #def initialize(new_book)
  #  puts new_book
    @id = id
    @title = title
    @author = author
  end

  # Returns this book's unique identification number.
  def get_id()

  end

  # Returns this book's title.
  def get_title()

  end

  # Returns this book's author.
  def get_author()

  end

  # Returns the date (as an integer) that this book is due.  
  def get_due_date()

  end

  # Sets the due date of this Book. Doesn't return anything.
  def check_out(due_date)

  end
  
  # Sets the due date of this Book to nil. Doesn't return anything.
  def check_in()

  end

  # Returns a string of the form "id: title, by author”.
  def to_s()

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
  end

  # Returns this member's name.
  def get_name()
    return @name
  end

  # Adds this Book object to the set of books checked out by this member.
  def check_out(book)

  end

  # Removes this Book object from the set of books checked out by this member. 
  # (Since members are usually said to "return" books, this method should be called return !)   
  def give_back(book)

  end

  # # Returns the set of Book objects checked out to this member (may be the empty set).
  def get_books()

  end

  # Tells this member that he/she has overdue books. (What the method actually 
  # does is just print out this member's name along with the notice.)
  def send_overdue_notice(notice)

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
    # • Read in, from a file named collection.txt, a list of (title,author) tuples, 
    # Create a Book from each tuple, and save these books in some appropriate data 
    # structure of your choice. Give each book a unique id number (starting from 1, not 0). 
    # You may have many copies of the "same" book (same title and author), but each will have its own id.
    @available_books = {}
    collection = File.new('collection.txt')
    
    i = 1;
    collection.each do |row|
      
      book = row.chomp.split(', ')
      unless book[0].empty?
        @available_books[i] = { :title => book[0], :author => book[1] }
        i = i + 1
      end
      #@books = Book.new(book[0], book[1])
    end
    
    puts @available_books.count
    
    @available_books.each do |book|
      puts book[:title]
      #puts " by "
      puts book
    end

    test = @available_books.map { |pair| pair.first }

    puts test

    # • Create a Calendar object (you should create exactly one of these).
    @cal = Calendar.new

    # • Define an empty dictionary of members. 
    #    - The keys will be the names of members and the values will be the corresponding Member objects.
    
    # • Set a flag variable to indicate that the library is not open.
    @open = false
    # • Set the current member (the one being served) to nil
    @current_member = nil
    @members = ['ross', 'bob']
  end

  # If the library is already open, raises an Exception with the message "The library is already open!".
  # Otherwise, starts the day by advancing the Calendar, and setting the flag to indicate that the
  # library is open. (). Returns: The string "Today is day n.”
  def open()
    if @open
      raise ArgumentError, "The library is already open!"
    else
      @open = true
      puts "Today is day #{@cal.get_date}" # + @cal.get_date
    end
  end

  # Prints a nicely formatted, multiline string, listing the names of members who have overdue books, and for each such member, the
  # books that are overdue. Or, the string "No books are overdue.”.
  def find_all_overdue_books()

  end

  # Issues a library card to the person with this name. However, no member should be permitted to have more than one library card.
  # Returns either "Library card issued to name_of_member." or "name_of_member already has a library card.”.
  # Possible Exception: "The library is not open.".
  def issue_card(name_of_member)

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
    end
    puts @members
  end

  # Prints a multiline string, each line containing one book (as returned by the book's to_s method), 
  # of the books that have been checked out by the member currently being served, and which are overdue. 
  # If the member has no overdue books, the string “None” is printed.
  # May throw an Exception with an appropriate message: 
  #  - "The library is not open.”
  #  - "No member is currently being served.”
  def find_overdue_books()

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
  def check_in(*book_numbers) # * = 1..n of book numbers
    if not @open
      puts "The library is not open."
    end
    if @current_member == nil
      puts "No member is currently being served."
    else
      puts "Checking in the books..."
      puts book_numbers
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

    # search (caseinsensitive) the list / array / hash


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
    end

  end

  # Shut down operations and go home for the night. None of the other operations (except quit) can be used 
  # when the library is closed. If successful, returns the string "Good night.".
  # May throw an Exception with the message: "The library is not open.”
  def close()
  
  end

  # The mayor, citing a budget crisis, has stopped all funding for the library. Can happen at any time. 
  # Returns the string "The library is now closed for renovations.”
  def quit()

  end

end

lib = Library.new
lib.open
lib.open
lib.serve('ross')
lib.check_in([1,2,3])
