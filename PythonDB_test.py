#!/usr/bin/python
# -*- coding: utf-8 -*-

# Homework 13: Python DB connection
# Author: Evangelos Giakoumakis
# Date: 04/09/2017

import sqlite3

try:
    # Create a database in RAM
    # db = sqlite3.connect(':memory:')
    
    # Creates or opens a file called mydb.db with a SQLite3 DB
    db = sqlite3.connect('mydb.db')
    # Get a cursor object
    cursor = db.cursor()
    # Check if table users does not exist and create it
    cursor.execute('''CREATE TABLE IF NOT EXISTS
                      users(id INTEGER PRIMARY KEY, name TEXT, phone TEXT, email TEXT unique, password TEXT)''')
    # Commit the change
    db.commit()    
    # user 1 
    name1 = 'Andres'
    phone1 = '3366858'
    email1 = 'user@example.com'
    password1 = '12345'
 
    # user 2 
    name2 = 'John'
    phone2 = '5557241'
    email2 = 'johndoe@example.com'
    password2 = 'abcdef'
 
    # Insert user 1
    cursor.execute('''INSERT INTO users(name, phone, email, password)
                  VALUES(?,?,?,?)''', (name1,phone1, email1, password1))
    print('First user inserted')
 
    # Insert user 2
    cursor.execute('''INSERT INTO users(name, phone, email, password)
                  VALUES(?,?,?,?)''', (name2,phone2, email2, password2))
    print('Second user inserted')
    
    # Create user 3
    name3 = 'Evan'
    phone3 = '9999999'
    email3 = 'egiakoumakis@example.com'
    password3 = '12345'

    # Insert user 3
    cursor.execute('''INSERT INTO users(name, phone, email, password)
                      VALUES(?,?,?,?)''', (name3, phone3, email3, password3))
    print('Third user inserted')
    
    db.commit()
    
    
    # Write SQL statement to select name, email, phone from user
    cursor.execute('''[Fill In]''')
    
    user1 = cursor.fetchone() #retrieve the first row
    print(user1[0]) #Print the first column retrieved(user's name)
    
    #retrieve all rows
    all_rows = cursor.fetchall()
    for row in all_rows:
        # row[0] returns the first column in the query (name), 
        # row[1] returns phone column.
        # row[2] returns email column.
        print('{0} : {1}, {2}'.format(row[0], row[1], row[2]))
    
    
    # Retrieve data with conditions, use again the "?" placeholder:
    user_id = 3
    cursor.execute('''SELECT name, email, phone FROM users WHERE id=?''', (user_id,))
    user = cursor.fetchone()
    print(user1) #Print the all columns of the first row 
    
    # Fill in SQL statement using UPDATE users for phone WHERE id is user_id using "?" placeholder
    newphone = '3432916'
    cursor.execute('''[Fill In]''', (newphone, user_id))
    db.commit() #Commit the change
    
    #   Fill in the SQL statement to INSERT INTO users for the columnes name, phone, email, password using
    #    "?" placeholders for the following user 1
    name1 = 'AB'
    phone1 = '3433001'
    email1 = 'abc@smu.edu'
    password1 = '123456'
    
    with db:
            db.execute('''[Fill In]''', (name1,phone1, email1, password1))
            
    cursor.execute('''SELECT * FROM users''')
    all_rows = cursor.fetchall()
    # print all columns of all_rows using for loop shown above
    [Fill In]    
            
# Catch the exception
except sqlite3.IntegrityError:
    print('Record already exists')
except Exception as e:
    # Roll back any change if something goes wrong
    db.rollback()
    raise e
finally:
    # Close the db connection
    db.close()
