/* SPDX-License-Identifier: MIT */
// Classic Malbolge output synthesis. The VM tables are from Olmstead's
// public-domain specification. The search and REXX transport are MIT.
// https://www.lscheffer.com/malbolge_spec.html
//
// After the bootstrap, C stays three cells ahead of D. Each step encrypts
// the instruction just executed and queues it as a future operand. Thus the
// same tape holds both code and data, without a separate initialized data row.
#include <algorithm>
#include <array>
#include <cstdint>
#include <iostream>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

const std::string encrypt =
    "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1C"
    "B6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@";
enum Opcode { jump = 4, output = 5, input = 23, rotate_right = 39,
              move_data = 40, operate = 62, no_op = 68, halt = 81 };
constexpr std::array<int, 8> legal{
    jump, output, input, rotate_right, move_data, operate, no_op, halt};
constexpr std::array<int, 5> search_ops{
    output, rotate_right, operate, no_op, input};
constexpr int ncell = 59049;
constexpr int period = 94;

int source(int phase, int op) {
    return (op - phase + 2 * period - 33) % period + 33;
}

int crazy(int a, int v) {
    constexpr int table[3][3] = {{1,0,0}, {1,0,2}, {2,2,1}};
    int out = 0;
    for (int pow = 1; pow < ncell; pow *= 3, a /= 3, v /= 3)
        out += pow * table[v % 3][a % 3];
    return out;
}

int rotate(int x) { return x / 3 + x % 3 * 19683; }

struct State {
    int pos = 0;
    int a = 0;
    uint64_t tape = 0;
    int root = 0;
    uint64_t path = 0;
    int depth = 0;

    // Remaining behavior depends on the operand queue, A and C modulo 94.
    uint64_t key() const { return (tape << 23) | (uint64_t(pos % period) << 16) | a; }
};

struct Prior {
    bool operator()(const State& a, const State& b) const {
        if (a.pos != b.pos) return a.pos > b.pos;
        if (a.depth != b.depth) return a.depth < b.depth;
        return a.key() > b.key();
    }
};

struct Trace {
    std::size_t parent;
    int end;
    uint64_t path;
    int length;
};

struct Boundary {
    State cursor;
    std::size_t trace;
};

std::string reconstruct(const std::string& boot, const std::vector<Trace>& trace,
                        std::size_t current, int end) {
    std::string program(end + 1, '\0');
    std::copy(boot.begin(), boot.end(), program.begin());
    while (current) {
        const auto& step = trace[current];
        for (int i = 0; i < step.length; ++i) {
            const int address = step.end - step.length + i;
            const int op = search_ops[(step.path >> (3 * i)) & 7];
            program[address] = char(source(address % period, op));
        }
        current = step.parent;
    }
    program[end] = char(source(end % period, halt));
    return program;
}

void print_rexx(const std::string& program) {
    const std::string alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-";
    std::cout << "/* REXX */\np=0\nalphabet='" << alphabet
              << "'\nops='4 5 23 39 40 62 68 81'\n";
    auto digit = [&](std::size_t address) {
        int op = (program[address] + address) % period;
        return std::find(legal.begin(), legal.end(), op) - legal.begin();
    };
    for (std::size_t start = 0; start < program.size(); start += 420) {
        const auto end = std::min(start + 420, program.size());
        std::cout << "call emit '";
        for (auto i = start; i < end; i += 2)
            std::cout << alphabet[8 * digit(i) + (i+1 < end ? digit(i+1) : 0)];
        std::cout << "'," << end - start << '\n';
    }
    std::cout << R"(exit
emit: procedure expose p alphabet ops
parse arg packed,count
line=''
do i=1 to count
  v=pos(substr(packed,(i-1)%2+1,1),alphabet)-1
  if i//2=1 then x=v%8
  else x=v//8
  b=(word(ops,x+1)-p-33)//94
  if b<0 then b=b+94
  line=line||d2c(b+33)
  if length(line)=70 then do
    say line
    line=''
  end
  p=(p+1)//94
end
if line<>'' then say line
return
)";
}

int main(int argc, char** argv) {
    const bool raw = argc == 2 && std::string(argv[1]) == "--raw";
    if (argc != 1 && !raw) {
        std::cerr << "usage: mg [--raw] < PAYLOAD\n";
        return 1;
    }
    constexpr int lag = 3;
    constexpr int beam = 24;
    constexpr int depth = 13;
    constexpr int slack = 3;
    static_assert(3 * (depth + 1) <= 64);
    const uint64_t mask = (uint64_t(1) << (8 * lag)) - 1;

    // Start with MOVD (M[0]=40), then step until a legal
    // ASCII pointer sets C to D+lag. The jump encrypts its target.
    int boot_d = 41;
    for (; boot_d + lag < 127; ++boot_d) {
        const int op = (2 * boot_d + lag) % period;
        if (std::find(legal.begin(), legal.end(), op) != legal.end()) break;
    }
    if (boot_d + lag >= 127) return 3;
    const int code_start = boot_d + lag + 1;
    std::string boot(code_start, '\0');
    for (int i = 0; i < code_start; ++i) boot[i] = source(i, no_op);
    boot[0] = source(0, move_data);
    boot[boot_d - 40] = source(boot_d - 40, jump);
    boot[boot_d] = boot_d + lag;
    uint64_t tape = 0;
    for (int i = 1; i <= lag; ++i) {
        int value = boot[boot_d + i];
        if (i == lag) value = encrypt[value - 33];
        tape |= uint64_t(value) << (8 * (i - 1));
    }

    std::vector<Trace> trace(1);
    std::vector<Boundary> frontier{{{code_start, 0, tape}, 0}};
    // Keep several shortest prefixes at each output boundary. A prefix that
    // leaves a different A or operand queue may encode the next byte better.
    int round = 0;
    for (int target; (target = std::cin.get()) != EOF; ++round) {
        std::priority_queue<State, std::vector<State>, Prior> pending;
        std::unordered_map<uint64_t, int> best;
        std::unordered_set<uint64_t> finished;
        std::vector<Boundary> next;
        for (int root = 0; root < int(frontier.size()); ++root) {
            State seed = frontier[root].cursor;
            seed.root = root;
            seed.depth = 0;
            seed.path = 0;
            pending.push(seed);
            best[seed.key()] = seed.pos;
        }
        while (!pending.empty()) {
            State state = pending.top();
            pending.pop();
            if (best[state.key()] < state.pos) continue;
            if (!next.empty() && state.pos >= next.front().cursor.pos + slack) break;
            const int phase = state.pos % period;
            const int data = state.tape & 255;
            for (std::size_t k = 0; k < search_ops.size(); ++k) {
                const int op = search_ops[k];
                if (op == output && state.a % 256 != target) continue;
                if (op != output && state.depth >= depth) continue;
                const int enc = encrypt[source(phase, op) - 33];
                State dest = state;
                dest.tape = ((state.tape >> 8) | (uint64_t(enc) << (8 * (lag - 1)))) & mask;
                dest.pos++;
                dest.path |= uint64_t(k) << (3 * dest.depth++);
                if (op == output) {
                    if (!finished.insert(dest.key()).second) continue;
                    trace.push_back({frontier[dest.root].trace, dest.pos, dest.path, dest.depth});
                    next.push_back({dest, trace.size() - 1});
                    if (int(next.size()) == beam) break;
                    continue;
                }
                if (op == rotate_right) dest.a = rotate(data);
                if (op == operate) dest.a = crazy(state.a, data);
                // The relay supplies an empty stream. IN sets A to the
                // language's EOF value without receiving any payload bytes.
                if (op == input) dest.a = ncell - 1;
                const auto prev = best.find(dest.key());
                if (prev != best.end() && prev->second <= dest.pos) continue;
                best[dest.key()] = dest.pos;
                pending.push(dest);
            }
            if (int(next.size()) == beam) break;
        }
        if (next.empty()) {
            std::cerr << "stopped at byte " << round << ", best length " << frontier.front().cursor.pos << "\n";
            return 2;
        }
        frontier = std::move(next);
    }
    if (std::cin.bad()) {
        std::cerr << "malbolge-encode: failed to read the payload\n";
        return 1;
    }
    const auto& first = frontier.front();
    if (first.cursor.pos + 1 > ncell) {
        std::cerr << "malbolge-encode: " << first.cursor.pos + 1
                  << " cells exceeds " << ncell << "\n";
        return 2;
    }
    const auto program = reconstruct(boot, trace, first.trace, first.cursor.pos);
    if (raw) {
        for (std::size_t i = 0; i < program.size(); i += 70)
            std::cout << program.substr(i, 70) << '\n';
    } else {
        print_rexx(program);
    }
    return std::cout.flush() ? 0 : 1;
}
