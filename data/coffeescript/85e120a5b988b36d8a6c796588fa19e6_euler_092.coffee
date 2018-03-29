###
A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

For example,

44 -> 32 -> 13 -> 10 -> 1 -> 1
85 -> 89 -> 145 -> 42 -> 20 -> 4 -> 16 -> 37 -> 58 -> 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
###

print = -> console.log(arguments...)

sod = (n) ->
    sum = 0
    for c in String(n)
        i = parseInt(c)
        sum += i*i
    return sum

res_nums = {}
res_nums[89] = true

add_to_res = (nums) ->
    for n of nums
         res_nums[n] = true

for i in [0...1e7]
    nums = {}

    n = i
    loop
        if res_nums[n]
            add_to_res(nums)
            break
        if nums[n]
            break
        nums[n] = true
        n = sod(n)


print (i for i of res_nums).length, res_nums
