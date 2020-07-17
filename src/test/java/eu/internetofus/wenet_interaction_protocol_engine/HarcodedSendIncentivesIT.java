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
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
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

package eu.internetofus.wenet_interaction_protocol_engine;

import static eu.internetofus.common.components.service.WeNetServiceSimulators.waitUntilCallbacks;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.incentive_server.IncentiveTest;
import eu.internetofus.common.components.interaction_protocol_engine.WeNetInteractionProtocolEngine;
import eu.internetofus.common.components.service.TextualMessage;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;

/**
 * Integration test over the hard coded send incentives.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class HarcodedSendIncentivesIT {

  /**
   * Check that an incentive message is send to the user.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  public void shouldSendIncentiveMessage(final Vertx vertx, final VertxTestContext testContext) {

    new IncentiveTest().createModelExample(1, vertx, testContext, testContext.succeeding(incentive -> {

      incentive.IncentiveType = "Message";
      incentive.Badge = null;
      WeNetInteractionProtocolEngine.createProxy(vertx).sendIncentive(incentive, testContext.succeeding(sent -> {

        waitUntilCallbacks(incentive.AppID, callbacks -> {

          for (int i = 0; i < callbacks.size(); i++) {

            final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
            if (message != null && "".equals(message.title.trim()) && incentive.UserId.equals(message.recipientId) && incentive.Message.content.equals(message.text)) {

              return true;

            }
          }

          return false;

        }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> testContext.completeNow()));

      }));

    }));

  }

  /**
   * Check that an incentive badge is send to the user.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  public void shouldSendIncentiveBadge(final Vertx vertx, final VertxTestContext testContext) {

    new IncentiveTest().createModelExample(1, vertx, testContext, testContext.succeeding(incentive -> {

      incentive.IncentiveType = "Badge";
      incentive.Message = null;
      WeNetInteractionProtocolEngine.createProxy(vertx).sendIncentive(incentive, testContext.succeeding(sent -> {

        waitUntilCallbacks(incentive.AppID, callbacks -> {

          for (int i = 0; i < callbacks.size(); i++) {

            final TextualMessage message = Model.fromJsonObject(callbacks.getJsonObject(i), TextualMessage.class);
            if (message != null && "".equals(message.title.trim()) && incentive.UserId.equals(message.recipientId) && incentive.Badge.Message.equals(message.text)) {

              return true;

            }
          }

          return false;

        }, vertx, testContext).onComplete(testContext.succeeding(callbacks -> testContext.completeNow()));

      }));

    }));

  }

}
