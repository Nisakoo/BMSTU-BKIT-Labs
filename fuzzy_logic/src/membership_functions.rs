pub trait MembershipFunction {
    fn apply(&self, value: f64) -> f64;
}

pub struct RightZMF {
    pub lower_bound: f64,
    pub upper_bound: f64,
}

impl RightZMF {
    pub fn new(lower_bound: f64, upper_bound: f64) -> Self{
        RightZMF { lower_bound: lower_bound, upper_bound: upper_bound }
    }
}

impl MembershipFunction for RightZMF {
    fn apply(&self, value: f64) -> f64 {
        if value <= self.lower_bound {
            return 0.0;
        } else if value >= self.upper_bound {
            return 1.0;
        }

        return (value - self.lower_bound) / (self.upper_bound - self.lower_bound);
    }
}

pub struct LeftZMF {
    pub lower_bound: f64,
    pub upper_bound: f64,
}

impl LeftZMF {
    pub fn new(lower_bound: f64, upper_bound: f64) -> Self{
        LeftZMF { lower_bound: lower_bound, upper_bound: upper_bound }
    }
}

impl MembershipFunction for LeftZMF {
    fn apply(&self, value: f64) -> f64 {
        if value <= self.lower_bound {
            return 1.0;
        } else if value >= self.upper_bound {
            return 0.0;
        }

        return (self.upper_bound - value) / (self.upper_bound - self.lower_bound);
    }
}

pub struct TriangaleMF {
    pub top: f64,
    pub lower_bound: f64,
    pub upper_bound: f64,
}

impl TriangaleMF {
    pub fn new(top: f64, lower_bound: f64, upper_bound: f64) -> Self{
        TriangaleMF {top: top, lower_bound: lower_bound, upper_bound: upper_bound}
    }
}

impl MembershipFunction for TriangaleMF {
    fn apply(&self, value: f64) -> f64 {
        if value > self.lower_bound && value <= self.top {
            return (self.top - value) / (self.top - self.lower_bound);
        } else if value < self.upper_bound && value > self.top {
            return (self.upper_bound - value) / (self.upper_bound - self.top);
        }

        return 0.0;
    }
}