import React from 'react';
import Grid from './Grid';
import './../static/styles/board.css';
import clockwise from './../static/images/clockwise.svg';
import counterclockwise from './../static/images/counterclockwise.svg';
import vertical from './../static/images/vertical.png';
import horizontal from './../static/images/horizontal.svg';

const Board = ({
  handleSelectGrid,
  board,
  handleMakeMove,
  handleSkipTurn,
  handleEndGame,
  rotatePiece,
  selectedPiece,
  selectedRow,
  selectedCol,
  currentTurn,
  isValidMove,
  winner,
  powerups,
}) => {
  let findPowerup = (row, col) => {
    // console.log('findPowerup');
    // console.log(row);
    // console.log(col);
    if (powerups) {
      let powerup = powerups.find((powerup) => {
        return powerup.row === row && powerup.col === col;
      });
      if (powerup) return powerup.powerup;
      else return null;
    }
    return null;
  };
  return (
    <div id='board'>
      {board.map((row, rowIndex) => {
        return (
          <div className='board-row'>
            {row.map((gridColor, colIndex) => {
              if (
                selectedPiece &&
                selectedRow !== -1 &&
                rowIndex <= selectedRow + selectedPiece.length - 1 &&
                rowIndex >= selectedRow &&
                colIndex <= selectedCol + selectedPiece.length - 1 &&
                colIndex >= selectedCol
              ) {
                if (
                  selectedPiece[rowIndex - selectedRow][
                    colIndex - selectedCol
                  ] === 1
                ) {
                  return (
                    <Grid
                      color={currentTurn}
                      handleClick={handleSelectGrid}
                      row={rowIndex}
                      column={colIndex}
                      translucent={true}
                      key={colIndex}
                      powerupText={findPowerup(rowIndex, colIndex)}
                    />
                  );
                }
              }
              return (
                <Grid
                  color={gridColor}
                  handleClick={handleSelectGrid}
                  row={rowIndex}
                  column={colIndex}
                  translucent={false}
                  key={colIndex}
                  powerupText={findPowerup(rowIndex, colIndex)}
                />
              );
            })}
          </div>
        );
      })}
      <span
        style={{
          color: 'red',
          marginTop: '2%',
          fontSize: '20px',
          visibility: isValidMove ? 'hidden' : 'visible',
        }}
      >
        Invalid move
      </span>
      <button onClick={handleMakeMove} style={{ marginTop: '2%' }}>
        Make Move
      </button>
      <button onClick={handleSkipTurn}>Skip</button>
      <button
        onClick={handleEndGame}
        style={{ backgroundColor: 'red', color: 'white' }}
      >
        End Game
      </button>
      <span
        style={{
          color: winner,
          marginTop: '2%',
          fontSize: '20px',
          visibility: winner ? 'visible' : 'hidden',
        }}
      >
        {winner} player has won the game!
      </span>
      <div style={{ display: 'flex', flexWrap: 'wrap', marginTop: '5%' }}>
        <button
          style={{ width: '50%' }}
          onClick={() => rotatePiece('clockwise')}
        >
          Rotate Clockwise
          <img
            src={clockwise}
            alt='clockwise'
            style={{ height: '5%', width: '5%' }}
          />
        </button>
        <button
          style={{ width: '50%' }}
          onClick={() => rotatePiece('counterclockwise')}
        >
          Rotate Counterclockwise
          <img
            src={counterclockwise}
            alt='counterclockwise'
            style={{ height: '5%', width: '5%' }}
          />
        </button>
        <button
          style={{ width: '50%' }}
          onClick={() => rotatePiece('vertical')}
        >
          Flip Vertically
          <img
            src={vertical}
            alt='counterclockwise'
            style={{ height: '4%', width: '4%' }}
          />
        </button>
        <button
          style={{ width: '50%' }}
          onClick={() => rotatePiece('horizontal')}
        >
          Flip Horizontally
          <img
            src={horizontal}
            alt='counterclockwise'
            style={{ height: '4%', width: '4%' }}
          />
        </button>
      </div>
    </div>
  );
};

export default Board;
