# Really short version

# settings.py
# Order of apps is given by app_sequence inside a dict in SESSION_CONFIGS

SESSION_CONFIGS = [
    dict(app_sequence=['Score', 'HighScorersOnly', 'Final']
         #...
         )]

# Participant values shared across apps have to be defined in PARTICIPANT_FIELDS, stored as a dict of values.
PARTICIPANT_FIELDS = ['score']

# Score/__init__.py

class ScorePage(Page):
    form_model = 'player'
    # Include the 'score' field in the form
    form_fields = ['score']

    # After submitting form, store the score on the global participant dict
    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.score = player.score

# HighScorersOnly/__init__.py

class MyPage(Page):
    form_model = 'player'
    # Display page only if the score was 5 or more
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return participant.score > 4

# Short version
#/usr/bin/env python3

# settings.py
# Order of apps is given by app_sequence inside a dict in SESSION_CONFIGS

# ...
SESSION_CONFIGS = [
    dict(app_sequence=['Score', 'HighScorersOnly', 'Final']
         #...
         )]

# Participant values shared across apps have to be defined in PARTICIPANT_FIELDS, stored as a dict of values.
PARTICIPANT_FIELDS = ['score']

# ...

# Score/__init__.py

# ...

class Player(BasePlayer):
    # Define the form field to enter the score.
    score = models.IntegerField(initial=0, label='Your score (from 0 to 10)', max=10, min=0)

class ScorePage(Page):
    form_model = 'player'
    # Include the 'score' field in the form
    form_fields = ['score']

    # After submitting form, store the score on the global participant dict
    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        participant = player.participant
        participant.score = player.score

# Skip this? It shows how to sequence pages inside an app.
page_sequence = [ScorePage]

# HighScorersOnly/__init__.py

class MyPage(Page):
    form_model = 'player'
    # Display page only if the score was 5 or more
    @staticmethod
    def is_displayed(player: Player):
        participant = player.participant
        return participant.score > 4

page_sequence = [MyPage]
