# pinky-ponky is a ping pong bot for [Slack](www.slack.com) 
written in Scala (uses the [slack-scala-client](https://github.com/gilbertw1/slack-scala-client) library)

It keeps track of leaderboards, and can help you organise tournaments!
With minor modifications it can also be used for other 2 player games.

# Getting started
Clone the repo, and create a file `src/main/resources/credentials.txt` and put in it:
* 1st line: slack token
* 2nd line: administrator name
* 3rd line: channel-name

eg.
```
aaaa-543265645634-aa4254fsdg34tgsg4t32gfdg
the.boss
games-channel
```

To get the slack token, go to `Apps & Integrations` on the slack menu, then `Manage`(top right in Slack's website), then choose `Custom Integrations` on the sidebar, then `Bots`, and finally click `Add configuration`.

# Run
If you haven't already, install [sbt](http://www.scala-sbt.org/). Then
In a terminal navigate to the root of the project and type

`sbt run`

# Usage

### Type `help` in a slack channel/private IM with the bot and you 'll get a reply with the following message: 

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/help.png" width="70%">

### To report a score, type `vs @opponent.name <your score>-<opponent score>`:

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/vs.png" width="70%">

Your opponent will receive the following message:

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/confirmation.png" width="70%">

Then you can reply `yes` or `no`, and you 'll both get a confirmation message:

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/confirmed.png" width="70%">

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/confirmed1.png" width="70%">

### To see the leaderboards, type `stats`:

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/stats.png" width="70%">

### To see your personal statistics, type `stats me` (or to see someone else's statistics type `stats @user.name`)

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/stats_me.png" width="70%">

### To see who has not confirmed/rejected the score reported by their opponent, type `pending`:

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/pending.png" width="70%">

### To register for a tournament (which is open for registration) type `register`

### To see the progress of the tournament type `tournament`. To see upcoming matches, type `next`:

<img src="https://github.com/tsanikgr/pinky-ponky/blob/master/doc/tournament.png" width="100%">

# Administrator commmands:

The administrator of the bot, declared in `credentials.txt`, can issue some additional commands.
See [Cmd.scala](src/main/scala/app/Cmd.scala)

### General
* `exit`: To stop the bot
* `reload`: To reload the `games.txt`, `pending.txt` and `tournament.txt`, which are the databases for games, pending results and tournaments (e.g. if you do a manual edit)
* `id @user.name`: To get the slack id of a player
* `message Your message goes here`: To send a message to the slack channel declared in `credentials.txt` as the slack bot.
* `result @player1 @player2 2 0`: Verify a pending result (see pending results by typing `pending`).
* `deletepending @player`: If player has not confirmed a result yet, you can delete the last score reported by their opponent like so.

### Tournament

* `new tournament`: Delete an existing tournament (if it exists) and start a new one. Tournament is now open for registration.
* `start`: Start the tournament, no more registrations allowed
* `stop`: Stop and delete a tournament.
* `register @user1 @user2 @user3`: Register players for the tournament (they can always register themselves by typing `register`
* `vst @user1 @user2 2 0`: Report a tournament result that will not be taken into account in the leaderboards.
* `delete @user1 @user2`: Delete a tournament result between user1 and user2
* `extend time_in_days`: Extend the deadline for all tournament matches - can also be negative (e.g. to terminate a game that will never happen early)
* `extend time_in_days @user1`: Extend the deadline for all tournament matches of user1
* `extend time_in_days @user1 @user2`: Extend the deadline for the tournament match between user1 and user2
