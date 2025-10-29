use crate::membership_functions::{LeftZMF, MembershipFunction, RightZMF, TriangaleMF};
use crate::fuzzy_set::{Set, FuzzySet};
use crate::defuzzification::{gravity_center};

mod membership_functions;
mod fuzzy_set;
mod defuzzification;

fn main() {
    let temperature = -7.0;
    let target_temperature = 22.0;
    let d = 3.0;

    let cold_mf = LeftZMF::new(-10.0, 10.0);
    let hot_mf = RightZMF::new(-10.0, 10.0);
    let target_mf = TriangaleMF::new(target_temperature, target_temperature - d, target_temperature + d);

    let target = Set::new(
        target_mf.apply(temperature)
    );

    let cold = Set::new(
        cold_mf.apply(temperature)
    );

    let hot = Set::new(
        hot_mf.apply(temperature)
    );

    let rule1 = cold.and(target.not());
    let rule2 = hot.and(target.not());

    let rule3 = rule1.or(rule2);

    println!("{}", rule3);

    let heating = RightZMF::new(-5.0, 10.0);
    let cooling = LeftZMF::new(-10.0, 5.0);

    let aggregate_function = |x: f64| {
        let heat = Set::new(heating.apply(x));
        let cool = Set::new(cooling.apply(x));

        rule1.and(heat).or(rule2.and(cool)).value
    };

    let result = gravity_center(
        &aggregate_function, -10.0, 10.0, 0.1
    );

    println!("{}", result);
}