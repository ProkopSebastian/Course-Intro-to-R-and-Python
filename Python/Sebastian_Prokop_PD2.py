### Data Processing in R and Python 2023Z
### Homework Assignment no. 2
###
### IMPORTANT
### This file should contain only solutions to tasks in the form of a functions
### definitions and comments to the code.
###
#
# Include imports here
import pandas as pd

# -----------------------------------------------------------------------------#
# Task 1
# -----------------------------------------------------------------------------#

def solution_1(Posts, Users):
    merged_data = pd.merge(Users[['Id', 'Location']], Posts[['OwnerUserId']], left_on='Id', right_on='OwnerUserId')
    filtered_data = merged_data[merged_data['Location'] != '']
    grouped_data = filtered_data.groupby('Location').size().reset_index(name='Count')
    sorted_data = grouped_data.sort_values(by='Count', ascending=False).head(10)

    # Żeby zgadzała się numeracja przy porównaniu za pomocą equals:
    sorted_data.reset_index(drop=True, inplace=True)

    return(sorted_data)


# -----------------------------------------------------------------------------#
# Task 2
# -----------------------------------------------------------------------------#

def solution_2(Posts, PostLinks):
    relatedTab = PostLinks.groupby(['RelatedPostId'])['RelatedPostId'].count().reset_index(name = "NumLinks")
    relatedTab.rename(columns={"RelatedPostId": "PostId"}, inplace=True)
    sol2 = pd.merge(relatedTab, Posts, left_on = 'PostId', right_on = 'Id')
    sol2 = sol2[sol2['PostTypeId'] == 1]
    # Poniższe sortowanie było odrobinę trudne do wychwycenia, bo jednakowe wartości 
    # w NumLinks pojawiają się dopiero w 19 wierszu. Oprócz sortowania po NumLinks
    # Trzeba jeszcze dodać sortowanie po tytule alfabetycznie, ale to i tak nie naprawia
    # wszystkich problemów -  przy porównywaniu będzie jeszcze jedno sortowanie
    sol2 = sol2.sort_values(by = ['NumLinks', 'Title'], ascending = [False, True])
    sol2 = sol2[['Title', 'NumLinks']]
    sol2.reset_index(drop = True, inplace = True)
    return(sol2)

# -----------------------------------------------------------------------------#
# Task 3
# -----------------------------------------------------------------------------#

def solution_3(Comments, Posts, Users):
    CmtToScr = Comments.groupby(['PostId'])['Score'].sum().reset_index(name = 'CommentsTotalScore')
    PostsBestComments = pd.merge(CmtToScr, Posts, left_on = 'PostId', right_on = 'Id')
    PostsBestComments = PostsBestComments[PostsBestComments['PostTypeId'] == 1]
    PostsBestComments = PostsBestComments[['OwnerUserId', 'Title', 'CommentCount', 'ViewCount', 'CommentsTotalScore']]
    sol3 = pd.merge(PostsBestComments, Users, left_on = 'OwnerUserId', right_on = 'Id')
    sol3 = sol3.sort_values(by = 'CommentsTotalScore', ascending = False)
    sol3 = sol3.head(10)
    sol3 = sol3[['Title', 'CommentCount', 'ViewCount', 'CommentsTotalScore', 'DisplayName', 'Reputation', 'Location']]
    sol3.reset_index(drop = True, inplace = True)
    return(sol3)
    
# -----------------------------------------------------------------------------#
# Task 4
# -----------------------------------------------------------------------------#

def solution_4(Posts, Users):
    answers = Posts[Posts["PostTypeId"] == 2]
    answers = answers.groupby(['OwnerUserId'])['OwnerUserId'].count().reset_index(name='AnswersNumber')
    questions = Posts[Posts['PostTypeId'] == 1]
    questions = questions.groupby('OwnerUserId')['OwnerUserId'].count().reset_index(name='QuestionsNumber')
    postsCounts = pd.merge(answers, questions, on = "OwnerUserId", how = 'inner')
    postsCounts = postsCounts[(postsCounts["AnswersNumber"] > postsCounts["QuestionsNumber"])]
    postsCounts = postsCounts.sort_values(by = "AnswersNumber", ascending = False).head(5)
    sol4 = pd.merge(postsCounts, Users, left_on="OwnerUserId", right_on="Id", how='left')
    sol4 = sol4[['DisplayName', 'QuestionsNumber', 'AnswersNumber', 'Location', 'Reputation', 'UpVotes', 'DownVotes']]
    return(sol4)
    
# -----------------------------------------------------------------------------#
# Task 5
# -----------------------------------------------------------------------------#

def solution_5(Posts, Users):
    answers = Posts[Posts["PostTypeId"] == 2]
    ansCount = answers.groupby(['ParentId'])['ParentId'].count().reset_index(name = "AnswersCount")
    postAuth = pd.merge(ansCount, Posts, left_on='ParentId', right_on='Id')
    postAuth = postAuth[['AnswersCount', 'Id', 'OwnerUserId']]
    sol5 = pd.merge(postAuth, Users, left_on='OwnerUserId', right_on='AccountId')
    sol5 = sol5.groupby(['OwnerUserId', 'AccountId', 'DisplayName', 'Location'], dropna=False)['AnswersCount'].mean().reset_index(name="AverageAnswersCount")
    sol5 = sol5[['AccountId', 'DisplayName', 'Location', 'AverageAnswersCount']]
    sol5 = sol5.sort_values(by=['AverageAnswersCount', 'AccountId'], ascending=[False, False])
    sol5 = sol5.head(10)
    sol5.reset_index(drop=True, inplace=True)
    return(sol5)
