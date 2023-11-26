import React, { useState } from 'react';
import Game from './game/Game';
import Header from './Header';
import './static/styles/App.css';

function App() {
  const [numPlayers, setNumPlayers] = useState('1');
  const [numAI, setNumAI] = useState('0');
  const [aiLevel, setAILevel] = useState('0');
  const [isPlayingGame, setIsPlayingGame] = useState(false);
  const [classicMode, setClassicMode] = useState(true);

  let handleSubmit = (event) => {
    event.preventDefault();
    setIsPlayingGame(true);
  };

  let handleSetNumPlayers = (event) => {
    setNumPlayers(event.target.value);
  };

  let handleSetNumAI = (event) => {
    setNumAI(event.target.value);
  };

  let handleSetAILevel = (event) => {
    setAILevel(event.target.value);
  };

  let handleSetClassicMode = (event) => {
    setClassicMode(event.target.value === 'classic');
  };

  return (
    <div>
      <Header />
      {isPlayingGame ? (
        <Game
          numPlayers={numPlayers}
          numAI={numAI}
          aiLevel={aiLevel}
          setIsPlayingGame={setIsPlayingGame}
          classicMode={classicMode}
        />
      ) : (
        <div>
          <h2 style={{ textAlign: 'center', marginBottom: '2%' }}>
            Please select a total of 2 or 4 players
          </h2>
          <form onSubmit={handleSubmit}>
            <div className='input-container'>
              <label>Enter the number of players: </label>
              <select onChange={handleSetNumPlayers}>
                <option value='1'>1</option>
                <option value='2'>2</option>
                <option value='3'>3</option>
                <option value='4'>4</option>
              </select>
            </div>
            <div className='input-container'>
              <label>Enter the number of AI players: </label>
              <select onChange={handleSetNumAI}>
                <option value='0'>0</option>
                <option value='1'>1</option>
                <option value='2'>2</option>
                <option value='3'>3</option>
              </select>
            </div>
            <div className='input-container'>
              <label>Select the AI level: </label>
              <select onChange={handleSetAILevel}>
                <option value='0'>0</option>
                <option value='1'>1</option>
                <option value='2'>2</option>
                <option value='3'>3</option>
                <option value='4'>4</option>
                <option value='5'>5</option>
              </select>
            </div>
            <div className='input-container'>
              <label>Classic</label>
              <input
                type='radio'
                name='classic-select'
                value='classic'
                checked={true}
                style={{ marginRight: '1%' }}
                onChange={handleSetClassicMode}
              ></input>
              <label>Non-Classic</label>
              <input
                type='radio'
                name='classic-select'
                value='non-classic'
                onChange={handleSetClassicMode}
              ></input>
            </div>
            <div id='button-container'>
              <button type='submit'>Start Game</button>
            </div>
          </form>
        </div>
      )}
    </div>
  );
}

export default App;
