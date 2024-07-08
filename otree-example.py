# settings.py
SESSION_CONFIGS = [dict(app_sequence=['Intro', 'Choices', 'Result'])]
PARTICIPANT_FIELDS = ['ok']

# Intro/IntroPage.html
# {{ block content }}
#   {{ next_button }}
# {{ endblock }}

# Choices/__init__.py
# ...
class Player(BasePlayer):
    choice = models.StringField(label='Your choice:')
class ChoicePage(Page):
    form_model = 'player'
    form_fields = ['choice']
    @staticmethod
    def before_next_page(player: Player, timeout_happened):
        player.participant.ok = player.choice == random.choice(['heads', 'tails'])
# ...

# Result/ResultPage.html
# {{ block content }}
#   {% if player.participant.ok %}
#     You chose right.
#   {% else %}
#     You chose wrong.
#   {% endif %}
# {{ endblock }}
