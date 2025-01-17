# from flask import Flask, request, jsonify
# from flask_cors import CORS
# import subprocess
# import os

# app = Flask(__name__)
# CORS(app)

# BASE_DIR = os.path.dirname(os.path.abspath(__file__))
# PROLOG_FILE = os.path.abspath(os.path.join(BASE_DIR, "../poker_ai.pl"))

# @app.route('/api/calculate', methods=['POST'])
# def calculate():
#     data = request.json
#     player_hand = data['playerHand']
#     board_cards = data['boardCards']
#     pot = data['pot']
#     opponent_bet = data['opponentBet']
#     bankroll = data['bankroll']

#     prolog_query = (
#         f"set_my_hand(hero, card({convert_card(player_hand[0])}), card({convert_card(player_hand[1])})).\n"
#         f"set_community_cards([{', '.join(['card(' + convert_card(c) + ')' for c in board_cards])}]).\n"
#         f"suggest_action_kelly(hero, 1000, {pot}, {opponent_bet}, {bankroll}, Action, Amount).\n"
#         f"winning_probability_monte_carlo(hero, 1000, Probability).\n"
#     )

#     result = run_prolog_query(prolog_query)
#     if not result:
#         return jsonify({"error": "Prolog computation failed"}), 500

#     parsed_result = parse_prolog_output(result)
#     if not parsed_result:
#         return jsonify({"error": "Prolog output parsing failed"}), 500

#     return jsonify({
#         "winRate": parsed_result.get('Probability', 0) * 100,
#         "aiDecision": parsed_result.get('Action', 'Unknown'),
#         "raiseAmount": parsed_result.get('Amount', 0)
#     })

# def run_prolog_query(query):
#     try:
#         print(f"DEBUG: Executing Prolog Query:\n{query}")

#         process = subprocess.Popen(
#             ["swipl"],
#             stdin=subprocess.PIPE,
#             stdout=subprocess.PIPE,
#             stderr=subprocess.PIPE,
#             text=True
#         )
#         full_query = f"consult('{PROLOG_FILE}').\n{query}\nhalt.\n"
#         output, error = process.communicate(full_query)

#         print(f"DEBUG: Prolog Output:\n{output}")
#         print(f"DEBUG: Prolog Error Output:\n{error}")

#         if process.returncode != 0 or not output.strip():
#             print(f"ERROR: Prolog execution failed with error: {error}")
#             return None
#         return output

#     except Exception as e:
#         print(f"ERROR: Prolog execution exception: {str(e)}")
#         return None

# def parse_prolog_output(output):
#     try:
#         print(f"DEBUG: Parsing Prolog Output:\n{output}")  # Debugging output
#         parsed_result = {}

#         # Split the output into lines for parsing
#         lines = [line.strip() for line in output.strip().split("\n") if line.strip()]
#         for line in lines:
#             # Parse Winning Probability
#             if line.startswith("Winning Probability:"):
#                 probability_value = line.split(":")[1].strip()
#                 try:
#                     parsed_result["Probability"] = float(probability_value)
#                 except ValueError:
#                     print(f"WARNING: Invalid Probability value: {probability_value}")
#                     parsed_result["Probability"] = 0.0

#             # Parse Action
#             elif line.startswith("Action:"):
#                 action_value = line.split(":")[1].strip()
#                 parsed_result["Action"] = action_value or "Unknown"

#             # Parse Amount
#             elif line.startswith("Amount:"):
#                 amount_value = line.split(":")[1].strip()
#                 try:
#                     parsed_result["Amount"] = int(amount_value)
#                 except ValueError:
#                     print(f"WARNING: Invalid Amount value: {amount_value}")
#                     parsed_result["Amount"] = 0

#         # Ensure all required fields are present
#         parsed_result.setdefault("Probability", 0.0)
#         parsed_result.setdefault("Action", "Unknown")
#         parsed_result.setdefault("Amount", 0)

#         print(f"DEBUG: Parsed Result: {parsed_result}")  # Debugging output
#         return parsed_result

#     except Exception as e:
#         print(f"Error parsing Prolog output: {e}")
#         return None


# def convert_card(card):
#     suit_map = {"♠": "spades", "♥": "hearts", "♦": "diamonds", "♣": "clubs"}
#     card = card.replace("️", "")
#     suit = suit_map[card[0]]
#     rank = card[1:]
#     return f"{suit},{rank}"

# if __name__ == '__main__':
#     app.run(host='0.0.0.0', port=5050, debug=True)


from flask import Flask, request, jsonify
from flask_cors import CORS
import subprocess
import os

app = Flask(__name__)
CORS(app)

BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROLOG_FILE = os.path.abspath(os.path.join(BASE_DIR, "../poker_ai.pl"))

@app.route('/api/calculate', methods=['POST'])
def calculate():
    data = request.json
    player_hand = data['playerHand']
    board_cards = data['boardCards']
    pot = data['pot']
    opponent_bet = data['opponentBet']
    bankroll = data['bankroll']

    # Build the Prolog query
    # "poker_ai.pl" must be your EXACT file name (or "poker_ai_monte_carlo_kelly.pl").
    prolog_query = (
        f"set_my_hand(hero, card({convert_card(player_hand[0])}), card({convert_card(player_hand[1])})).\n"
        f"set_community_cards([{', '.join(['card(' + convert_card(c) + ')' for c in board_cards])}]).\n"
        f"suggest_action_kelly(hero, 1000, {pot}, {opponent_bet}, {bankroll}, Action, Amount).\n"
        f"winning_probability_monte_carlo(hero, 1000, Probability).\n"
    )

    result = run_prolog_query(prolog_query)
    if not result:
        return jsonify({"error": "Prolog computation failed"}), 500

    parsed_result = parse_prolog_output(result)
    if not parsed_result:
        return jsonify({"error": "Prolog output parsing failed"}), 500

    return jsonify({
        "winRate": parsed_result.get('Probability', 0) * 100,
        "aiDecision": parsed_result.get('Action', 'Unknown'),
        "raiseAmount": parsed_result.get('Amount', 0)
    })

def run_prolog_query(query):
    try:
        print("DEBUG: Executing Prolog Query:\n", query)

        process = subprocess.Popen(
            ["swipl"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        # The crucial part: we load your Prolog file, run the query, then halt.
        # Make sure PROLOG_FILE matches the file name you actually have.
        full_query = f"consult('{PROLOG_FILE}').\n{query}\nhalt.\n"
        output, error = process.communicate(full_query)

        print("DEBUG: Prolog Output:\n", output)
        print("DEBUG: Prolog Error Output:\n", error)

        if process.returncode != 0 or not output.strip():
            print(f"ERROR: Prolog execution failed with error: {error}")
            return None
        return output

    except Exception as e:
        print(f"ERROR: Prolog execution exception: {str(e)}")
        return None

def parse_prolog_output(output):
    """
    Your Prolog prints lines like:
        Setting my hand to hearts 9 and hearts 10
        Setting community cards to spades 10, diamonds 10, clubs 10
        Winning Probability:
        1
        suggest_action_kelly called
        Action:
        Raise
        Amount:
        1000

    We'll detect e.g. "Winning Probability:" then look at the NEXT line for the numeric value.
    Same for "Action:" and "Amount:".
    """
    try:
        print("DEBUG: Parsing Prolog Output:\n", output)
        parsed_result = {}

        lines = [ln.strip() for ln in output.splitlines() if ln.strip()]
        i = 0
        while i < len(lines):
            line = lines[i]
            if line == "Winning Probability:":
                # The numeric probability is on the next line
                if i + 1 < len(lines):
                    val = lines[i+1].strip()
                    try:
                        parsed_result["Probability"] = float(val)
                    except ValueError:
                        parsed_result["Probability"] = 0.0
                    i += 2
                    continue

            elif line == "Action:":
                # The action is on the next line
                if i + 1 < len(lines):
                    parsed_result["Action"] = lines[i+1].strip() or "Unknown"
                    i += 2
                    continue

            elif line == "Amount:":
                # The amount is on the next line
                if i + 1 < len(lines):
                    amt_str = lines[i+1].strip()
                    try:
                        parsed_result["Amount"] = int(amt_str)
                    except ValueError:
                        parsed_result["Amount"] = 0
                    i += 2
                    continue

            # Not a recognized label, just move on
            i += 1

        # Ensure defaults
        parsed_result.setdefault("Probability", 0.0)
        parsed_result.setdefault("Action", "Unknown")
        parsed_result.setdefault("Amount", 0)

        print("DEBUG: Parsed Result:", parsed_result)
        return parsed_result

    except Exception as e:
        print(f"ERROR parsing Prolog output: {e}")
        return None

def convert_card(card):
    """
    Convert something like "♠10" => "spades,10"
    or "♥9" => "hearts,9"
    """
    suit_map = {"♠": "spades", "♥": "hearts", "♦": "diamonds", "♣": "clubs"}
    card = card.replace("️", "")  # remove hidden unicode
    suit = suit_map.get(card[0], "spades")
    rank = card[1:]
    return f"{suit},{rank}"

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5050, debug=True)
