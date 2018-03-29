function [bitstream, signal] = decode_signal_from_huffman_bitstream( bitstream, huffman_table, nr_of_symbols )
% function [bitstream, signal] = decode_signal_from_huffman_bitstream( bitstream, huffman_table, nr_of_symbols )
%
% function recreates original input signals from bitstream with help of huffman table
%
% Output Args:
% BITSTREAM: bitstream after huffman decoding
% SIGNAL: decoded signal
%
% Input Args:
% BISTREAM: bitstream to decode
% HUFFMAN_TABLE: huffman table
% NR_OF_SYMBOLS: number of symbols to decode (optional). if omitted, real max is used.

% Recreate original input signals from bit stream with help of Huffman table

if ( nargin < 3 )
    nr_of_symbols = 2^63;
end


nr_of_huffman_entries = size(huffman_table,1);
huffman_length = zeros(nr_of_huffman_entries,1);
for k = 1:nr_of_huffman_entries
    huffman_length(k) = size(huffman_table{k,1},2);
end
min_bits = min(huffman_length);
max_bits = max(huffman_length);
signal = [];
for k = 1:nr_of_symbols
    symbol_found = false;
    current_nr_of_bits = min_bits;
    while(current_nr_of_bits <= max_bits && symbol_found == false)
        [this_huffman new_bitstream] = bitstream_read_bits(bitstream, current_nr_of_bits);
        if (isempty(this_huffman) == 1)
            disp('Ende des Bitstreams:while');
            break;
        end
        symbol_row = find(huffman_length == current_nr_of_bits);
        for l = 1:size(symbol_row)
            if(isequal(this_huffman,huffman_table{symbol_row(l),1}) == 1)
                signal = [signal ; huffman_table{symbol_row(l),2}];
                bitstream = new_bitstream;
                symbol_found = true;
                break;
            end
        end
        current_nr_of_bits = current_nr_of_bits + 1;
%         disp(current_nr_of_bits);
%         if (current_nr_of_bits == 16)
%             disp('YEAH!');
%         end
    end
    
    if (isempty(this_huffman) == 1)
        disp('Ende des Bitstreams:for');
        break;
    end
end
end












% function [bitstream, signal] = decode_signal_from_huffman_bitstream( bitstream, huffman_table, nr_of_symbols )
% % function [bitstream, signal] = decode_signal_from_huffman_bitstream( bitstream, huffman_table, nr_of_symbols )
% %
% % function recreates original input signals from bitstream with help of huffman table
% %
% % Output Args:
% % BITSTREAM: bitstream after huffman decoding
% % SIGNAL: decoded signal
% %
% % Input Args:
% % BISTREAM: bitstream to decode
% % HUFFMAN_TABLE: huffman table
% % NR_OF_SYMBOLS: number of symbols to decode (optional). if omitted, real max is used.
%
% % Recreate original input signals from bit stream with help of Huffman table
%
%     if ( nargin < 3 )
%         nr_of_symbols = realmax;
%     end
%
%     nr_of_huffman_entries = size(huffman_table,1);
%     huffman_length = zeros(nr_of_huffman_entries,1);
%     for i = 1:nr_of_huffman_entries
%         huffman_length(i) = size(huffman_table{i,1},2);
%     end
%     min_bits = min(huffman_length);
%     max_bits = max(huffman_length);
%     signal = [];
%
%     for i = 1:nr_of_symbols
%         symbol_found = false;
%         current_nr_of_bits = min_bits;
%
%         while(current_nr_of_bits <= max_bits && symbol_found == false)
%             [this_huffman, new_bitstream] = bitstream_read_bits(bitstream, current_nr_of_bits);
%             if( isempty(this_huffman) )
%                 disp('End of bit stream reached: end of while-loop.');
%                 break;
%             end
%             for symbol_row = find(huffman_length == current_nr_of_bits)
%                 if (isempty(symbol_row) == 1)
%                     current_nr_of_bits = current_nr_of_bits + 1;
%                     symbol_row = find(huffman_length == current_nr_of_bits);
%                     [this_huffman, new_bitstream] = bitstream_read_bits(bitstream, current_nr_of_bits);
%                 end
%                 m = size(symbol_row,1);
%                 temp = 1;
%                 if( m > 1)
%                     for j = 1:m
%                         if (isequal(this_huffman, huffman_table{j,1}))
%                             temp = j;
%                             break;
%                         end
%                     end
%                     this_symbol = huffman_table{temp,2};
%                     signal = [signal; this_symbol];
%                     bitstream = new_bitstream;
%                     symbol_found = true;
%                     break;
%                 else
%                     if( this_huffman == huffman_table{symbol_row,1})
%                         %                 if( isequal(this_huffman, huffman_table{symbol_row,1}) )
%                         this_symbol = huffman_table{symbol_row,2};
%                         signal = [signal; this_symbol];
%                         bitstream = new_bitstream;
%                         symbol_found = true;
%                         break;
%                     end
%                 end
%             end
%             current_nr_of_bits = current_nr_of_bits+1;
%             disp(current_nr_of_bits);
%         end
%         if( isempty(this_huffman) )
%             disp('End of for-loop.');
%             break;
%         end
%         % break if no symbols found somewhere here
%     end
%
% %     if( size(signal,1) < nr_of_symbols )
% %         disp('End of Stream.');
% %     end
% end
