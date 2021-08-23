/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.components.interaction_protocol_engine.State;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test over the {@link StatesRepository}.
 *
 * @see StatesRepository
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class StatesRepositoryIT {

  /**
   * Verify that can not found a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#searchState(String, String,String)
   */
  @Test
  public void shouldNotFoundUndefinedState(final Vertx vertx, final VertxTestContext testContext) {

    testContext.assertFailure(StatesRepository.createProxy(vertx).searchState("undefined", "undefined", "undefined"))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Create an state that is unique.
   *
   * @return the created state.
   */
  protected State createUniqueState() {

    var state = new State();
    state.attributes = new JsonObject().put("key", "value");
    state.communityId = UUID.randomUUID().toString();
    state.taskId = UUID.randomUUID().toString();
    state.userId = UUID.randomUUID().toString();
    return state;

  }

  /**
   * Verify that can found a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#searchState(String, String,String)
   */
  @Test
  public void shouldFoundState(final Vertx vertx, final VertxTestContext testContext) {

    var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state)).onSuccess(storedState -> {
      testContext.assertComplete(
          StatesRepository.createProxy(vertx).searchState(state.communityId, state.taskId, state.userId)).onSuccess(

              foundState -> testContext.verify(() -> {
                assertThat(foundState).isEqualTo(storedState);
                testContext.completeNow();
              }));
    });

  }

  /**
   * Verify that can not store a state that can not be an object.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#storeState(State)
   */
  @Test
  public void shouldNotStoreAStateThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final State state = new State() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    testContext.assertFailure(StatesRepository.createProxy(vertx).storeState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can store a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#storeState(State)
   */
  @Test
  public void shouldStoreState(final Vertx vertx, final VertxTestContext testContext) {

    var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state))
        .onSuccess(storedState -> testContext.verify(() -> {

          assertThat(storedState).isEqualTo(state);
          testContext.completeNow();
        }));

  }

  /**
   * Verify that can not update a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldNotUpdateUndefinedState(final Vertx vertx, final VertxTestContext testContext) {

    var state = this.createUniqueState();
    testContext.assertFailure(StatesRepository.createProxy(vertx).updateState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can not update a state if it is not defined.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldNotUpdateAStateThatCanNotBeAnObject(final Vertx vertx, final VertxTestContext testContext) {

    final State state = new State() {

      /**
       * {@inheritDoc}
       */
      @Override
      public JsonObject toJsonObject() {

        return null;
      }
    };
    testContext.assertFailure(StatesRepository.createProxy(vertx).updateState(state))
        .onFailure(failed -> testContext.completeNow());

  }

  /**
   * Verify that can update a state.
   *
   * @param vertx       event bus to use.
   * @param testContext context that executes the test.
   *
   * @see StatesRepository#updateState(State)
   */
  @Test
  public void shouldUpdateState(final Vertx vertx, final VertxTestContext testContext) {

    var state = this.createUniqueState();
    testContext.assertComplete(StatesRepository.createProxy(vertx).storeState(state))
        .onSuccess(stored -> testContext.verify(() -> {

          final var update = Model.fromJsonObject(stored.toJsonObject(), State.class);
          update.attributes.put("key2", "value2");
          testContext.assertComplete(StatesRepository.createProxy(vertx).updateState(update).compose(
              empty -> StatesRepository.createProxy(vertx).searchState(state.communityId, state.taskId, state.userId)))
              .onSuccess(foundState -> testContext.verify(() -> {

                assertThat(foundState._creationTs).isEqualTo(stored._creationTs);
                assertThat(foundState._lastUpdateTs).isGreaterThanOrEqualTo(stored._lastUpdateTs);
                update._creationTs = stored._creationTs;
                update._lastUpdateTs = foundState._lastUpdateTs;
                assertThat(foundState).isEqualTo(update);
                testContext.completeNow();
              }));

        }));

  }

}
