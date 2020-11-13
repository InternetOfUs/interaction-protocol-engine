/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

import http from 'k6/http';
import { group, check } from 'k6';

/**
 * Test to check the performance of the interaction protocol engine.
 */
export default function() {

	var interactionProtocolEngineApi = 'https://wenet.u-hopper.com/dev/interaction_protocol_engine';
	if (typeof __ENV.INTERACTION_PROTOCOL_ENGINE_API === 'string') {

		interactionProtocolEngineApi = __ENV.INTERACTION_PROTOCOL_ENGINE_API;
	}
	group('interaction protocol engine performance', function() {

		group('send incentive', function() {

			var incentive = {
				name: {
					prefix: 'k6',
					first: 'performance',
					last: 'test'
				}
			};
			var payload = JSON.stringify(incentive);
			var params = {
				headers: {
					'Content-Type': 'application/json',
				}
			};
			var createResponse = http.post(interactionProtocolEngineApi + '/incentive', payload, params);
			check(createResponse, {
				'created profile': (r) => r.status === 201,
				'obtain created profile': (r) => {
					profile = r.json();
					return profile !== undefined;
				}
			});

		});

	});

}