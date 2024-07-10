module pokemon {
  import basicSpells.* from "./spells/basicSpells"
  import rareSpells.* from "./spells/rareSpells"

  type PokemonType = Water | Fire | Grass
  type DamageModifier = SuperEffective | NotVeryEffective | Normal

  type Pokemon = {
    name: str,
    hp: int,
    speed: int,
    team: str,
    pokemon_type: PokemonType
  }

  pure val POKEMONS = Set(
    { name: "Squirtle", hp: 44, speed: 43, team: "", pokemon_type: Water },
    { name: "Bulbasaur", hp: 45, speed: 45, team: "", pokemon_type: Grass },
    { name: "Charmander", hp: 49, speed: 65, team: "", pokemon_type: Fire }
  )

  var pokemons: str -> Pokemon
  var next_team: str

  pure def other_team(team: str): str = if (team == "A") "B" else "A"

  pure def damage(p: Pokemon, d: int): Pokemon = {
    { ...p, hp: p.hp - d }
  }

  pure def damage_modifier(attacker: Pokemon, receiver: Pokemon): DamageModifier = {
    match attacker.pokemon_type {
        | Water => match receiver.pokemon_type {
          | Water => NotVeryEffective
          | Fire => SuperEffective
          | Grass => NotVeryEffective
        }
        | Fire => match receiver.pokemon_type {
          | Water => NotVeryEffective
          | Fire => NotVeryEffective
          | Grass => SuperEffective
        }
        | Grass => match receiver.pokemon_type {
          | Water => SuperEffective
          | Fire => NotVeryEffective
          | Grass => NotVeryEffective
        }
      }
  }

  action tackle(receiver: Pokemon): bool = all {
    pokemons' = pokemons.setBy(receiver.team, p => p.damage(10))
  }

  action elemental_attack(attacker: Pokemon, receiver: Pokemon): bool = {
    val base_damage = 10
    val actual_damage = match damage_modifier(attacker, receiver) {
      | SuperEffective => base_damage * 2
      | NotVeryEffective => base_damage / 2
      | Normal => base_damage
    }

    pokemons' = pokemons.setBy(receiver.team, p => p.damage(actual_damage))
  }

  action attack(attacker: Pokemon, receiver: Pokemon): bool = {
    all {
      attacker != receiver,
      attacker.hp > 0,
      receiver.hp > 0,
      if (damage_modifier(attacker, receiver) == SuperEffective) {
        elemental_attack(attacker, receiver)
      } else {
        tackle(receiver)
      }
    }
  }

  action init = {
    nondet team_A_pokemon = POKEMONS.oneOf()
    nondet team_B_pokemon = POKEMONS.oneOf()
    all {
      pokemons' = Map(
        "A" -> { ...team_A_pokemon, team: "A" },
        "B" -> { ...team_B_pokemon, team: "B" }
      ),
      next_team' = if (team_A_pokemon.speed > team_B_pokemon.speed) "A" else "B",
    }
  }

  action step = {
    val attacker = pokemons.get(next_team)
    val receiver = pokemons.get(other_team(next_team))
    if (pokemons.values().forall(p => p.hp > 0)) all {
      attack(attacker, receiver),
      next_team' = other_team(next_team),
    } else all {
      pokemons' = pokemons,
      next_team' = next_team,
    }
  }

  val charmander_cant_beat_squirtle =
    (pokemons.values().exists(p => p.pokemon_type == Fire) and pokemons.values().exists(p => p.pokemon_type == Water))
      implies
    pokemons.values().filter(p => p.pokemon_type == Water).forall(p => p.hp > 0)
}
