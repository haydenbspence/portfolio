import psycopg2
from nltk.corpus import words as nltk_words
import syllables
import random
import sys
import Levenshtein as lev
from nltk.corpus import wordnet
import nltk
from wordfreq import zipf_frequency

def get_wordnet_pos(treebank_tag):
    if treebank_tag.startswith('J'):
        return 3  # Adjective
    elif treebank_tag.startswith('V'):
        return 2  # Verb
    elif treebank_tag.startswith('N'):
        return 1  # Noun
    elif treebank_tag.startswith('R'):
        return 4  # Adverb
    else:
        return 0  # Other

def tag_pos(word):
    tagged_word = nltk.pos_tag([word])  # This will return a list with one tuple: [(word, pos)]
    _, pos = tagged_word[0]  # Extract the pos from the tuple
    return get_wordnet_pos(pos)

def fetch_potential_words(n):
    english_words = set(nltk_words.words())
    potential_words = [word for word in english_words if 4 <= len(word) <= 7]
    return random.sample(potential_words, n)

def remove_offensive(word_list):
    english_words = set(nltk_words.words())
    return [word for word in word_list if word in english_words]

def remove_similar(word_list):
    word_list.sort(key=len)  # sort by length
    result = []
    for word in word_list:
        if not any(lev.ratio(word, w) > 0.8 for w in result):  # if similar word not in result
            result.append(word)
    return result

def remove_words_with_less_syllables(word_list):
    return [word for word in word_list if syllables.estimate(word) > 1]

def remove_less_common(word_list, min_zipf=3.0):
    return [(word, zipf_frequency(word, 'en')) for word in word_list if zipf_frequency(word, 'en') >= min_zipf]

def main():
    # WARN: Only use explicit vars for local testing, store in env var and salt password for prod
    # Use openssl to salt a password:  openssl passwd -1 -salt {your_secret_salt} {your_secret_password}
    # Or even better use passlib and bcrypt or argon2
    conn = psycopg2.connect(database="word_db", user="user", password="password", host="localhost", port="port")
    cur = conn.cursor()

    cur.execute("SELECT word FROM words_start")
    words_start = [row[0] for row in cur.fetchall()]

    cur.execute("SELECT word FROM words_banned")
    words_banned = [row[0] for row in cur.fetchall()]

    for _ in range(int(sys.argv[1])):  # run the script for a specified number of times
        words_start = [word.lower() for word in words_start]  # convert to lower case
        words_start = list(set(words_start) - set(words_banned))  # remove banned words

        words_start += fetch_potential_words(1000)

        words_start = remove_offensive(words_start)
        words_start = remove_similar(words_start)
        words_start = remove_words_with_less_syllables(words_start)
        words_start = remove_less_common(words_start)

        for word, freq in words_start:
            pos = tag_pos(word)  # Get part of speech for the word
            cur.execute("INSERT INTO words_start (word, pos, freq) VALUES (%s, %s, %s)", (word, pos, freq))
            
        conn.commit()

    # Delete duplicate rows from the table
    cur.execute("""
        DELETE FROM words_start
        WHERE ctid NOT IN (
            SELECT MAX(ctid)
            FROM words_start
            GROUP BY word, pos, freq
        )
    """)
    
    # Delete rows where 'pos' or 'freq' are null
    cur.execute("""
        DELETE FROM words_start
        WHERE pos IS NULL OR freq IS NULL OR pos = 0
    """)
    
    conn.commit()

    cur.close()
    conn.close()

if __name__ == "__main__":
    main()
