import React, { useState, useEffect } from 'react';
import Board from './Board.js';
import PiecesDisplay from './PiecesDisplay';
import Player from './../utils/player.js';
import './../static/styles/game.css';

const Game = ({
  numPlayers,
  numAI,
  aiLevel,
  setIsPlayingGame,
  classicMode,
}) => {
  const [board, setBoard] = useState([[]]);
  const [currentTurn, setCurrentTurn] = useState('');
  const [selectedPiece, setSelectedPiece] = useState(null);
  const [selectedRow, setSelectedRow] = useState(-1);
  const [selectedCol, setSelectedCol] = useState(-1);
  const [isValidMove, setIsValidMove] = useState(true);
  const [players, setPlayers] = useState([]);
  const [winner, setWinner] = useState(null);
  const [powerups, setPowerups] = useState([]);

  useEffect(() => {
    let headers = new Headers();
    headers.append('Content-Type', 'application/json');
    headers.append('Accept', 'application/json');
    headers.append('Origin', 'http://localhost:3000');

    const data = {
      num_players: numPlayers,
      num_ai: numAI,
      ai_level: aiLevel,
      classic_mode: classicMode,
    };

    console.log(data);

    fetch('http://localhost:5000/start', {
      method: 'POST',
      mode: 'cors',
      headers: headers,
      body: JSON.stringify(data),
    })
      .then((response) => response.json())
      .then((data) => {
        console.log(data);
        if (data.error) {
          setIsPlayingGame(false);
        } else {
          setBoard(data.board);
          setCurrentTurn(data.current_turn);
          setPowerups(data.powerups);
          let players = data.players;
          setPlayers(
            players.map((player) => new Player(player.color, player.pieces))
          );
        }
      });
  }, []);

  let handleSelectGrid = (rowIndex, colIndex) => {
    if (selectedPiece) {
      setSelectedRow(rowIndex);
      setSelectedCol(colIndex);
    }
  };

  let handleSelectPiece = (piece) => {
    setSelectedPiece(piece);
  };

  let handleMakeMove = () => {
    if (selectedPiece && selectedCol !== -1 && selectedRow !== -1) {
      let headers = new Headers();
      headers.append('Content-Type', 'application/json');
      headers.append('Accept', 'application/json');
      headers.append('Origin', 'http://localhost:3000');

      const body = {
        piece: selectedPiece,
        row: selectedRow,
        col: selectedCol,
      };

      fetch('http://localhost:5000/makeMove', {
        method: 'POST',
        mode: 'cors',
        headers: headers,
        body: JSON.stringify(body),
      })
        .then((response) => response.json())
        .then((data) => {
          if (!data.error) {
            setSelectedRow(-1);
            setSelectedCol(-1);
            setSelectedPiece(null);
            setBoard(data.board);
            setCurrentTurn(data.current_turn);
            setIsValidMove(true);
            let players = data.players;
            setPlayers(
              players.map((player) => new Player(player.color, player.pieces))
            );
            if (data.is_ai) {
              console.log('AI SHOULD MAKE MOVE HERE');
              setTimeout(() => {
                getAIMove();
              }, 1500);
            }
          } else {
            // need to indicate on UI someway
            setIsValidMove(false);
          }
        });
    }
  };

  let getAIMove = () => {
    fetch('http://localhost:5000/makeAIMove', {
      mode: 'cors',
      method: 'GET',
    })
      .then((response) => response.json())
      .then((data) => {
        setBoard(data.board);
        setCurrentTurn(data.current_turn);
        let players = data.players;
        setPlayers(
          players.map((player) => new Player(player.color, player.pieces))
        );
        if (data.is_ai) {
          console.log('AI SHOULD MAKE MOVE HERE');
          setTimeout(() => {
            getAIMove();
          }, 1500);
        }
      });
  };

  let handleSkipTurn = () => {
    let headers = new Headers();
    headers.append('Content-Type', 'application/json');
    headers.append('Accept', 'application/json');
    headers.append('Origin', 'http://localhost:3000');

    fetch('http://localhost:5000/skipTurn', {
      mode: 'cors',
      method: 'GET',
      headers: headers,
    })
      .then((response) => response.json())
      .then((data) => {
        if (!data.error) {
          setCurrentTurn(data.current_turn);
          setSelectedPiece(null);
          setSelectedRow(-1);
          setSelectedCol(-1);
        }
        if (data.is_ai) {
          setTimeout(() => {
            getAIMove();
          }, 1500);
        }
      });
  };

  let rotatePiece = (transformation) => {
    let headers = new Headers();
    headers.append('Content-Type', 'application/json');
    headers.append('Accept', 'application/json');
    headers.append('Origin', 'http://localhost:3000');
    const data = {
      piece: selectedPiece,
      transformation: transformation,
    };
    fetch('http://localhost:5000/rotatePiece', {
      method: 'POST',
      mode: 'cors',
      body: JSON.stringify(data),
      headers: headers,
    })
      .then((response) => response.json())
      .then((data) => {
        let players = data.players;
        setSelectedPiece(null);
        setPlayers(
          players.map((player) => new Player(player.color, player.pieces))
        );
      });
  };

  let handleEndGame = () => {
    let headers = new Headers();
    headers.append('Content-Type', 'application/json');
    headers.append('Accept', 'application/json');
    headers.append('Origin', 'http://localhost:3000');

    fetch('http://localhost:5000/endGame', {
      mode: 'cors',
      method: 'GET',
      headers: headers,
    })
      .then((response) => response.json())
      .then((data) => setWinner(data.winner));
  };

  return (
    <div id='game-container'>
      <PiecesDisplay
        players={players.slice(0, players.length / 2)}
        key='left'
        handleSelectPiece={handleSelectPiece}
        currentTurn={currentTurn}
      />
      <Board
        handleSelectGrid={handleSelectGrid}
        board={board}
        handleMakeMove={handleMakeMove}
        handleSkipTurn={handleSkipTurn}
        handleEndGame={handleEndGame}
        rotatePiece={rotatePiece}
        selectedPiece={selectedPiece}
        selectedRow={selectedRow}
        selectedCol={selectedCol}
        currentTurn={currentTurn}
        isValidMove={isValidMove}
        winner={winner}
        powerups={powerups}
      />
      <PiecesDisplay
        players={players.slice(players.length / 2)}
        key='right'
        handleSelectPiece={handleSelectPiece}
        currentTurn={currentTurn}
      />
    </div>
  );
};

export default Game;
