import requests
import os
from vaderSentiment import sentiment

user_file = open("E:/Touchkin/NLP/Sleep/freeText.txt","r")
output_file = open("E:/Touchkin/NLP/Sleep/result_freetext.txt","w")
string = user_file.readline()
string = user_file.readline()

def remove_non_ascii(text):
    return ''.join(i for i in text if ord(i)<128)

while string:
	# msg_senti = sentiment("u " + string)
	# message_sentiment = msg_senti["compound"]
	# if message_sentiment > 0:
	# 	output = string + "<" + "POSITIVE" + "\n"
	# elif message_sentiment == 0:
	# 	output = string + "<" + "NEUTRAL" + "\n"
	# else:
	# 	output = string + "<" + "NEGATIVE" + "\n"

	msg_senti = sentiment("u " + string)
	message_sentiment = msg_senti["compound"]
	if message_sentiment > 0:
		output = "POSITIVE" + "\n"
	elif message_sentiment == 0:
		output = "NEUTRAL" + "\n"
	else:
		output = "NEGATIVE" + "\n"

	output_file.write(output)
	string = user_file.readline()

user_file.close()
output_file.close()