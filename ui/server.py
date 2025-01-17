from flask import Flask, request, jsonify
from flask_cors import CORS
import subprocess
import os 
import subprocess

app = Flask(__name__, static_folder="static", template_folder="templates")  # 让 Flask 托管前端文件

# 获取 `server.py` 所在的目录
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PROLOG_FILE = os.path.abspath(os.path.join(BASE_DIR, "../poker_ai.pl"))
print(f"DEBUG: Prolog 文件路径: {PROLOG_FILE}")

app = Flask(__name__)
CORS(app)  # 允许跨域请求
def home():
    return render_template('index.html')  # 加载你的 HTML

# 处理前端的计算请求
@app.route('/api/calculate', methods=['POST'])
def calculate():
    data = request.json
    player_hand = data['playerHand']  # e.g. ["♥️9", "♥️10"]
    board_cards = data['boardCards']  # e.g. ["♠️10", "♦️10", "♣️10"]
    pot = data['pot']
    opponent_bet = data['opponentBet']
    bankroll = data['bankroll']

    # 将数据转换为 Prolog 需要的格式
    prolog_query = f"set_my_hand(hero, card({convert_card(player_hand[0])}), card({convert_card(player_hand[1])})).\n"
    prolog_query += f"set_community_cards([{', '.join(['card(' + convert_card(c) + ')' for c in board_cards])}]).\n"
    prolog_query += f"suggest_action_kelly(hero, 1000, {pot}, {opponent_bet}, {bankroll}, Action, Amount).\n"
    prolog_query += f"winning_probability_monte_carlo(hero, 1000, Probability).\n"

    # **调试输出**
    print(f"DEBUG: 生成的 Prolog 查询:\n{prolog_query}")
    print("--------------------\n")

    # 调用 Prolog 计算
    result = run_prolog_query(prolog_query)

    if not result:
        return jsonify({"error": "Prolog 计算失败"}), 500

    # 解析 Prolog 输出
    parsed_result = parse_prolog_output(result)
    
    if not parsed_result:
        return jsonify({"error": "Prolog 输出解析失败"}), 500

    return jsonify({
        "winRate": parsed_result.get('Probability', 0) * 100,
        "aiDecision": parsed_result.get('Action', '未知'),
        "raiseAmount": parsed_result.get('Amount', 0)
    })

# Run Prolog query without halting immediately after execution
# 执行 Prolog 查询并加载文件


def run_prolog_query(query):
    try:
        print(f"DEBUG: 执行 Prolog 查询: \n{query}")

        # Start SWI-Prolog process
        process = subprocess.Popen(
            ["swipl"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,  # Ensure text mode for string output
            universal_newlines=True
        )

        # Construct the full Prolog script
        full_query = f"consult('{PROLOG_FILE}').\n{query}\nhalt.\n"

        # Send query to Prolog
        output, error = process.communicate(full_query)

        # Debugging output
        print(f"DEBUG: Prolog 返回: {output}")
        print(f"DEBUG: Prolog 错误输出: {error}")

        if process.returncode != 0:
            print(f"DEBUG: Prolog 计算出错: {error}")
            return None
        
        return output

    except Exception as e:
        print(f"DEBUG: Prolog 计算出错: {str(e)}")
        return None

# 解析 Prolog 输出
def parse_prolog_output(output):
    try:
        print(f"DEBUG: 解析 Prolog 输出:\n{output}")  # 调试输出
        parsed_result = {}

        # 逐行解析
        lines = output.strip().split("\n")
        action_found = False
        amount_found = False
        probability_found = False

        for i, line in enumerate(lines):
            # 解析 Action
            if "Action:" in line and not action_found:
                if i + 1 < len(lines):  # 确保下一行存在
                    action_value = lines[i + 1].strip().replace("'", "")
                    parsed_result["Action"] = action_value
                    action_found = True  # 避免重复解析

            # 解析 Amount
            if "Amount:" in line and not amount_found:
                if i + 1 < len(lines):  # 确保下一行存在
                    amount_value = lines[i + 1].strip().split()[0]  # 只取第一个数值
                    parsed_result["Amount"] = int(amount_value)  # 转换成整数
                    amount_found = True  # 避免重复解析
        
        # **解析 `Winning Probability`**
        for i, line in enumerate(lines):
            if "Winning Probability:" in line and not probability_found:
                if i + 1 < len(lines):  # 确保下一行存在
                    probability_value = lines[i + 1].strip().split()[0]
                    try:
                        parsed_result["Probability"] = float(probability_value)
                        probability_found = True
                    except ValueError:
                        print(f"WARNING: Probability 值转换失败: {probability_value}")

        print(f"DEBUG: 解析后的 Prolog 输出: {parsed_result}")  # 调试输出
        return parsed_result

    except Exception as e:
        print(f"Prolog 输出解析失败: {e}")
        return None



# 转换扑克牌格式 "♥️9" -> "hearts,9"
def convert_card(card):
    suit_map = {"♠": "spades", "♥": "hearts", "♦": "diamonds", "♣": "clubs"}
    card = card.replace("️", "")  # **去除可能的 Unicode 变体**
    print(f"DEBUG: card = {card}, card[0] = {card[0]}")  # 添加调试输出
    suit = suit_map[card[0]]
    rank = card[1:]
    print(f"DEBUG: 转换后的卡片 suit={suit}, rank={rank}")  # **打印调试信息**
    return f"{suit},{rank}"

if __name__ == '__main__':
   app.run(host='0.0.0.0', port=5050, debug=True)
