
--[[

Callable neural network process for use from Haskell

]]--

require 'torch'
require 'nn'
require 'nngraph'
require 'optim'
require 'lfs'
require 'sys'

require 'util.OneHot'
require 'util.misc'

cmd = torch.CmdLine()
cmd:text()
cmd:text('Sample from a character-level language model')
cmd:text()
cmd:text('Options')
-- required:
cmd:argument('-model','model checkpoint to use for sampling')
-- optional parameters
cmd:option('-gpuid',0,'which gpu to use. -1 = use CPU')
cmd:text()

-- parse input params
opt = cmd:parse(arg)

if opt.gpuid >= 0 then
    -- print('using CUDA on GPU ' .. opt.gpuid .. '...')
    require 'cutorch'
    require 'cunn'
    cutorch.setDevice(opt.gpuid + 1) -- note +1 to make it 0 indexed! sigh lua
end

-- load the model checkpoint
if not lfs.attributes(opt.model, 'mode') then
    io.stderr:write('Error: File ' .. opt.model .. ' does not exist. Are you sure you didn\'t forget to prepend cv/ ?\n')
end
checkpoint = torch.load(opt.model)

local vocab = checkpoint.vocab
local ivocab = {}
for c,i in pairs(vocab) do ivocab[i] = c end

protos = checkpoint.protos
local rnn_idx = #protos.softmax.modules - 1
opt.rnn_size = protos.softmax.modules[rnn_idx].weight:size(2)

-- initialize the rnn state
local current_state, state_predict_index
local model = checkpoint.opt.model

-- print('creating an LSTM...')
local num_layers = checkpoint.opt.num_layers or 1 -- or 1 is for backward compatibility
current_state = {}
for L=1,checkpoint.opt.num_layers do
    -- c and h for all layers
    local h_init = torch.zeros(1, opt.rnn_size)
    if opt.gpuid >= 0 then h_init = h_init:cuda() end
    table.insert(current_state, h_init:clone())
    table.insert(current_state, h_init:clone())
end
state_predict_index = #current_state -- last one is the top h
local seed_text = opt.primetext
local prev_char

protos.rnn:evaluate() -- put in eval mode so that dropout works properly

-- Test flattening and unflattening
local flat = flatten(current_state)
assert(torch.all(torch.eq(flat,flatten(unflatten(current_state,flat)))))

-- Print a state, including softmax
function write_state(state)
  local probs = torch.exp(protos.softmax:forward(state[state_predict_index]))
  state = flatten(state)
  probs = flatten(probs)

  local buf = torch.MemoryFile('w')
  buf:binary()
  local n = 2*8+4*(state:nElement()+probs:nElement())
  writeActualLong(buf,n)
  writeActualLong(buf,state:nElement())
  buf:writeFloat(state:storage())
  writeActualLong(buf,probs:nElement())
  buf:writeFloat(probs:storage())
  local s = string.sub(buf:storage():string(),1,-2)
  assert(#s == n+8)
  io.write(s)
end

function writeActualLong(buf,n)
  -- There is no native torch way to write a 64-bit int, so we fake it
  -- Note little endianness.
  buf:writeInt(n)
  buf:writeInt(0)
end

function preamble()
  -- Print network information
  -- WARNING: If this changes, quibble/RNN.hs must change accordingly
  local buf = torch.MemoryFile('w')
  buf:binary()
  local vocab_chars = {}
  local vocab_indices = {}
  for c,i in pairs(vocab) do -- Warning: quadratic time
    table.insert(vocab_chars,string.byte(c))
    table.insert(vocab_indices,i)
  end
  local n = 2*(8+#vocab_chars)
  writeActualLong(buf,n)
  writeActualLong(buf,#vocab_chars)
  buf:writeByte(torch.ByteStorage(vocab_chars))
  writeActualLong(buf,#vocab_indices)
  buf:writeByte(torch.ByteStorage(vocab_indices))
  local s = string.sub(buf:storage():string(),1,-2)
  assert(n+8 == #s,'expected '..(n+8)..', got '..#s)
  io.write(s)
  write_state(current_state)
  io.flush()
end

local stateSize = flatten(current_state):nElement()
preamble()

function process()
  -- Accept next,state pairs and compute new states
  while true do
    -- Process a command
    local cmd = io.read(16+4*(1+stateSize))
    if not cmd then
      return
    end
    --local start = sys.clock()
    buf = torch.MemoryFile(torch.CharStorage():string(cmd..'\0'),'r')
    buf:binary()
    buf:readInt(4) -- Skip 16 bytes (two longs)
    cmd = torch.FloatTensor(buf:readFloat(1+stateSize))
    local char = string.char(cmd[1])
    local state = unflatten(current_state,cmd[{{2,1+stateSize}}])

    -- Advance RNN
    local mid0 = sys.clock()
    local emb = protos.embed:forward(torch.Tensor{vocab[char]})
    state = protos.rnn:forward{emb,unpack(state)}
    local mid1 = sys.clock()

    -- Write new state
    write_state(state)
    --local stop = sys.clock()
    --io.stderr:write('cycle = ' .. (stop - start) .. ', nn ' .. (mid1 - mid0) .. '\n')
    io.flush()
  end
end

--pcall(process)
process()
