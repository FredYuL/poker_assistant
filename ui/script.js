let selectedCardId = null;
let playerHand = ["?", "?"];
let boardCards = ["?", "?", "?", "?", "?"];
let clickCount = 0;  // 记录点击“计算胜率”次数

// 🎯 选择牌（点击牌时触发）
function selectCard(cardId) {
    if (document.getElementById(cardId).classList.contains("locked")) return;
    selectedCardId = cardId;
    document.getElementById("selection-popup").style.display = "block";
}

// 🎯 确认牌的选择
function confirmSelection() {
    let suit = document.getElementById("suit").value;
    let rank = document.getElementById("rank").value;

    if (suit === "" || rank === "") {
        alert("请选择花色和数字！");
        return;
    }

    // 在 UI 上更新牌面
    document.getElementById(selectedCardId).innerText = suit + rank;

    // 记录数据
    if (selectedCardId.startsWith("player-card")) {
        let index = selectedCardId === "player-card-1" ? 0 : 1;
        playerHand[index] = suit + rank;
    } else {
        let index = parseInt(selectedCardId.split("-")[2]) - 1;
        boardCards[index] = suit + rank;
    }

    // 关闭选择窗口
    document.getElementById("selection-popup").style.display = "none";
}

// 🎯 计算胜率 & 发送数据到服务器
function sendToServer() {
    if (playerHand.includes("?")) {
        alert("请先选择你的手牌！");
        return;
    }

    clickCount++;

    if (clickCount === 1) {
        unlockCards(3);  // 翻牌圈 (Flop)
    } else if (clickCount === 2) {
        unlockCards(4);  // 转牌圈 (Turn)
    } else if (clickCount === 3) {
        unlockCards(5);  // 河牌圈 (River)
    }

    if (clickCount < 3 || boardCards.includes("?")) {
        return;
    }

    let btn = document.getElementById("calculate-btn");
    btn.innerText = "计算中...";
    btn.disabled = true;

    let pot = parseInt(document.getElementById("pot").value) || 0;
    let opponentBet = parseInt(document.getElementById("opponent-bet").value) || 0;
    let bankroll = parseInt(document.getElementById("bankroll").value) || 1000;

    // 过滤掉未选择的牌（"?"）
    let filteredBoardCards = boardCards.filter(card => card !== "?");

    // 🚀 **调试: 打印即将发送的数据**
    console.log("🟢 发送数据:", {
        playerHand: playerHand,
        boardCards: filteredBoardCards,
        pot: pot,
        opponentBet: opponentBet,
        bankroll: bankroll
    });

    // 🔹 发送数据到后端
    fetch("http://127.0.0.1:5000/api/calculate", { // 确保端口和后端一致
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
            playerHand: playerHand, 
            boardCards: filteredBoardCards, 
            pot: pot, 
            opponentBet: opponentBet, 
            bankroll: bankroll
        })
    })
    .then(response => response.json())
    .then(data => {
        console.log("✅ 服务器返回:", data);

        // **确保 `winRate` 正确更新**
        if (data.winRate !== undefined) {
            document.getElementById("win-rate").innerText = data.winRate.toFixed(2) + "%";
        } else {
            document.getElementById("win-rate").innerText = "计算失败";
        }

        // **更新 AI 建议**
        if (data.aiDecision !== undefined) {
            document.getElementById("ai-action").innerText = data.aiDecision;
        } else {
            document.getElementById("ai-action").innerText = "未知";
        }

        btn.innerText = "计算胜率";
        btn.disabled = false;
    })
    .catch(error => {
        console.error("❌ Fetch 请求失败:", error);
        alert("计算失败，请检查控制台错误信息！");
        btn.innerText = "计算胜率";
        btn.disabled = false;
    });
}

// 🔓 **解锁指定数量的公共牌**
function unlockCards(count) {
    for (let i = 0; i < count; i++) {
        document.getElementById(`board-card-${i + 1}`).classList.remove("locked");
    }
}

// 🎯 处理 bankroll、pot、bet 选择框的加减操作
function adjustValue(inputId, delta) {
    let input = document.getElementById(inputId);
    let currentValue = parseInt(input.value) || 0;
    input.value = Math.max(0, currentValue + delta);
}
