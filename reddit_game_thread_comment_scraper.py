import pandas as pd # to create pandas data
import praw # to connect to Reddit

# replace with your own info
reddit = praw.Reddit(client_id=r.client_id_reddit, 
                     client_secret=r.client_secret_reddit, 
                     user_agent=r.user_agent_reddit)

# grab the thread that you put in the url argument in the Inputs section
submission = reddit.submission(url=r.reddit_url)

# create an empty dictionary to insert our for-loop results in
topics_dict = {"body":[], "created":[]}

# request the body (text) and created (time of post) elements from our submission (the url we provided above)
submission.comments.replace_more(limit=None)
for comment in submission.comments.list():
  topics_dict["body"].append(comment.body)
  topics_dict["created"].append(comment.created)

# convert the dictionary into a data frame
topics_data = pd.DataFrame(topics_dict)
