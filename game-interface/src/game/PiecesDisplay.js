import React from 'react';
import Piece from './Piece.js';
import './../static/styles/pieces-display.css';

const PiecesDisplay = ({ players, handleSelectPiece, currentTurn }) => {
  return (
    <div className='pieces-display'>
      {players[0] !== undefined
        ? players.map((player) => {
            return (
              <div className='player-container' style={{ margin: '5%' }}>
                {player.pieces.map((piece) => {
                  return (
                    <Piece
                      piece={piece}
                      color={player.color}
                      handleSelectPiece={handleSelectPiece}
                      currentTurn={currentTurn}
                      key={piece}
                    />
                  );
                })}
              </div>
            );
          })
        : ''}
    </div>
  );
};

export default PiecesDisplay;
