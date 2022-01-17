package com.stc.cacheapi.controllers;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/v1/counters/{db_index}/{counter}")
public class CounterTrackingController {

}
