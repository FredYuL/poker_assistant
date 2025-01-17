// let selectedCardId = null;
// let playerHand = ["?", "?"];
// let boardCards = ["?", "?", "?", "?", "?"];
// let clickCount = 0;

// function selectCard(cardId) {
//     if (document.getElementById(cardId).classList.contains("locked")) return;
//     selectedCardId = cardId;
//     document.getElementById("selection-popup").style.display = "block";
// }

// function confirmSelection() {
//     let suit = document.getElementById("suit").value;
//     let rank = document.getElementById("rank").value;

//     if (suit === "" || rank === "") {
//         alert("Please select a suit and rank!");
//         return;
//     }

//     // Update card on UI
//     document.getElementById(selectedCardId).innerText = `${suit}${rank}`;

//     // Update data
//     if (selectedCardId.startsWith("player-card")) {
//         let index = selectedCardId === "player-card-1" ? 0 : 1;
//         playerHand[index] = `${suit}${rank}`;
//     } else {
//         let index = parseInt(selectedCardId.split("-")[2]) - 1;
//         boardCards[index] = `${suit}${rank}`;
//     }

//     document.getElementById("selection-popup").style.display = "none";
// }

// function sendToServer() {
//     if (playerHand.includes("?")) {
//         alert("Please select your hole cards first!");
//         return;
//     }

//     clickCount++;
//     if (clickCount === 1) unlockCards(3); 
//     else if (clickCount === 2) unlockCards(4); 
//     else if (clickCount === 3) unlockCards(5); 

//     // --- If you DO NOT want to wait for a full 5-card board, 
//     // --- comment out or remove the next line:
//     // if (clickCount < 3 || boardCards.includes("?")) return;

//     const btn = document.getElementById("calculate-btn");
//     btn.innerText = "Calculating...";
//     btn.disabled = true;

//     const pot = parseInt(document.getElementById("pot").value) || 0;
//     const opponentBet = parseInt(document.getElementById("opponent-bet").value) || 0;
//     const bankroll = parseInt(document.getElementById("bankroll").value) || 1000;

//     // Filter out "?" from the board
//     const filteredBoardCards = boardCards.filter((card) => card !== "?");

//     console.log("🟢 Sending Data:", {
//         playerHand: playerHand,
//         boardCards: filteredBoardCards,
//         pot: pot,
//         opponentBet: opponentBet,
//         bankroll: bankroll
//     });

//     // Ensure your Python is listening on 5050
//     fetch("http://127.0.0.1:5050/api/calculate", {
//         method: "POST",
//         headers: { "Content-Type": "application/json" },
//         body: JSON.stringify({
//             playerHand: playerHand,
//             boardCards: filteredBoardCards,
//             pot: pot,
//             opponentBet: opponentBet,
//             bankroll: bankroll,
//         }),
//     })
//         .then((response) => response.json())
//         .then((data) => {
//             console.log("✅ Server returned:", data);
//             if (data.winRate !== undefined) {
//                 document.getElementById("win-rate").innerText = data.winRate.toFixed(2) + "%";
//             } else {
//                 document.getElementById("win-rate").innerText = "Calculation failed";
//             }

//             if (data.aiDecision !== undefined) {
//                 document.getElementById("ai-action").innerText = data.aiDecision;
//             } else {
//                 document.getElementById("ai-action").innerText = "Unknown";
//             }

//             btn.innerText = "Calculate Win Rate";
//             btn.disabled = false;
//         })
//         .catch((error) => {
//             console.error("❌ Fetch failed:", error);
//             alert("Calculation failed. Check console!");
//             btn.innerText = "Calculate Win Rate";
//             btn.disabled = false;
//         });
// }

// function unlockCards(count) {
//     for (let i = 0; i < count; i++) {
//         document.getElementById(`board-card-${i + 1}`).classList.remove("locked");
//     }
// }
let selectedCardId = null;
let playerHand = ["?", "?"];
let boardCards = ["?", "?", "?", "?", "?"];
let clickCount = 0;

// When you click a card box, open the selection popup
function selectCard(cardId) {
    if (document.getElementById(cardId).classList.contains("locked")) return;
    selectedCardId = cardId;
    document.getElementById("selection-popup").style.display = "block";
}

// Confirm suit/rank from the popup, update UI and data
function confirmSelection() {
    let suit = document.getElementById("suit").value;
    let rank = document.getElementById("rank").value;

    if (suit === "" || rank === "") {
        alert("Please select a suit and rank!");
        return;
    }

    // Update the UI text
    document.getElementById(selectedCardId).innerText = `${suit}${rank}`;

    // Store in our arrays
    if (selectedCardId.startsWith("player-card")) {
        // It's one of the player's hole cards
        let index = (selectedCardId === "player-card-1") ? 0 : 1;
        playerHand[index] = `${suit}${rank}`;
    } else {
        // It's a board card
        let index = parseInt(selectedCardId.split("-")[2]) - 1;
        boardCards[index] = `${suit}${rank}`;
    }

    // Close popup
    document.getElementById("selection-popup").style.display = "none";
}

/**
 * Called when we click "Calculate Win Rate"
 * 1) Increments the clickCount to simulate dealing the next street (flop/turn/river).
 * 2) Potentially unlocks more board cards.
 * 3) Sends a POST request to the backend with the current game state.
 * 4) Updates the UI with the results (winRate, action, raiseAmount).
 */
function sendToServer() {
    // Must have both hole cards
    if (playerHand.includes("?")) {
        alert("Please select your hole cards first!");
        return;
    }

    // Simulate dealing streets
    clickCount++;
    if (clickCount === 1) unlockCards(3); // flop
    else if (clickCount === 2) unlockCards(4); // turn
    else if (clickCount === 3) unlockCards(5); // river

    // If you want to ONLY compute once the entire 5-card board is set:
    // if (clickCount < 3 || boardCards.includes("?")) return;

    const btn = document.getElementById("calculate-btn");
    btn.innerText = "Calculating...";
    btn.disabled = true;

    // Read user inputs for pot, opponentBet, bankroll
    const pot = parseInt(document.getElementById("pot").value) || 0;
    const opponentBet = parseInt(document.getElementById("opponent-bet").value) || 0;
    const bankroll = parseInt(document.getElementById("bankroll").value) || 1000;

    // Filter out any "?" placeholders in the board
    const filteredBoardCards = boardCards.filter((card) => card !== "?");

    // Debugging info in browser console
    console.log("🟢 Sending Data:", {
        playerHand: playerHand,
        boardCards: filteredBoardCards,
        pot: pot,
        opponentBet: opponentBet,
        bankroll: bankroll
    });

    // Fetch call to our server (assuming it runs on port 5050)
    fetch("http://127.0.0.1:5050/api/calculate", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
            playerHand: playerHand,
            boardCards: filteredBoardCards,
            pot: pot,
            opponentBet: opponentBet,
            bankroll: bankroll,
        }),
    })
        .then((response) => response.json())
        .then((data) => {
            console.log("✅ Server returned:", data);

            // Show the win rate
            if (data.winRate !== undefined) {
                document.getElementById("win-rate").innerText = data.winRate.toFixed(2) + "%";
            } else {
                document.getElementById("win-rate").innerText = "Calculation failed";
            }

            // If the AI decides "Raise," also show the raise amount
            if (data.aiDecision) {
                if (data.aiDecision === "Raise") {
                    // e.g. "Raise 500"
                    document.getElementById("ai-action").innerText =
                        data.aiDecision + " " + data.raiseAmount;
                } else {
                    // e.g. "Check", "Fold", or "Unknown"
                    document.getElementById("ai-action").innerText = data.aiDecision;
                }
            } else {
                document.getElementById("ai-action").innerText = "Unknown";
            }

            // Re-enable the button
            btn.innerText = "Calculate Win Rate";
            btn.disabled = false;
        })
        .catch((error) => {
            console.error("❌ Fetch failed:", error);
            alert("Calculation failed. Check console!");
            btn.innerText = "Calculate Win Rate";
            btn.disabled = false;
        });
}

// Unlocks the given number of board cards so we can click to set them
function unlockCards(count) {
    for (let i = 0; i < count; i++) {
        document.getElementById(`board-card-${i + 1}`).classList.remove("locked");
    }
}
