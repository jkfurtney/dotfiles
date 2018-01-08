import requests
import os.path
auth = ("jkfurtney", "3XVbvamDbdTTkn4THged")
url = "https://api.bitbucket.org/1.0/user/repositories"
data = requests.get(url, auth=auth).json()

for repo in data:
    name = repo[u'slug']
    if os.path.isdir(name+".git"):
        print 'git -C "{}.git/" fetch'.format(name)
    else:
        print "git clone git@bitbucket.org:jkfurtney/{}.git --mirror".format(name)


# from glob import glob

# for repo in glob("*.git"):
#     #print 'sh -c "cd {} && git pull"'.format(repo)
#     print 'git -C "{}/" fetch'.format(repo)
