Task: Token transfer

1. Write a specification of a token transfer protocol with the following properties:

    - There are 4 addresses and 3 different tokens. Of the 4 addresses, one is distinguised ("charity"), 3 belong to users
    - Initially, each user controls an equal, nonzero amount of each of the tokens
    - Users are able to perform the following actions:
        1. Burn:
            - Prerequisite: none
            - Effect: An arbitrary amount of tokens from that user's address is removed from the system
        2. Transfer to user: 
            - Prerequisite: recipient is another user
            - Effect: An arbitrary amount of tokens from that user's address is transferred to the recipient
        3. Donate:
            - Prerequisite: none
            - Effect: An arbitrary amount of tokens from that user's address is transferred to "charity"
        4. Celebrate:
            - Prerequisite: the "charity" address holds at least 20% of the initial supply of each of the tokens
            - Effect: all other actions become disabled

Is it ever possible to celebrate? If so, find an example.
Is it always possible to celebrate? If not, find a counterexample. (Hint: Treat this question as a thought experiment, you have not yet been given the tools to answer it.)
